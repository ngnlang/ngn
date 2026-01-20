use crossbeam_channel::{Receiver, Sender};
use std::sync::{Arc, OnceLock};

pub type BlockingJob = Box<dyn FnOnce() + Send + 'static>;

#[derive(Debug)]
pub struct BlockingPool {
    sender: Sender<BlockingJob>,
}

#[allow(dead_code)]
impl BlockingPool {
    pub fn new(threads: usize, queue_capacity: usize) -> Self {
        let (sender, receiver) = crossbeam_channel::bounded::<BlockingJob>(queue_capacity);
        let receiver = Arc::new(receiver);

        for _ in 0..threads {
            let rx = receiver.clone();
            std::thread::spawn(move || worker_loop(rx));
        }

        Self { sender }
    }

    /// Submit a job to the pool.
    ///
    /// This is fail-fast when the queue is full.
    pub fn try_submit(&self, job: BlockingJob) -> Result<(), ()> {
        self.sender.try_send(job).map_err(|_| ())
    }
}

fn worker_loop(rx: Arc<Receiver<BlockingJob>>) {
    while let Ok(job) = rx.recv() {
        job();
    }
}

static GLOBAL_POOL: OnceLock<BlockingPool> = OnceLock::new();
static GLOBAL_CPU_POOL: OnceLock<BlockingPool> = OnceLock::new();

#[allow(dead_code)]
pub fn global_blocking_pool() -> &'static BlockingPool {
    GLOBAL_POOL.get_or_init(|| {
        // For blocking/IO-ish work we allow more threads than cores because these
        // jobs often spend time waiting (network, disk, process waits, etc.).
        let threads = (num_cpus::get().max(1) * 4).min(64);
        let queue_capacity = 1024;
        BlockingPool::new(threads, queue_capacity)
    })
}

#[allow(dead_code)]
pub fn global_cpu_pool() -> &'static BlockingPool {
    GLOBAL_CPU_POOL.get_or_init(|| {
        // CPU-bound work should be kept near the core count to avoid excessive
        // contention and context switching.
        let threads = num_cpus::get().max(1);
        let queue_capacity = 1024;
        BlockingPool::new(threads, queue_capacity)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{
        Barrier,
        atomic::{AtomicBool, Ordering},
    };

    #[test]
    fn pool_queue_limit_is_enforced() {
        let pool = BlockingPool::new(1, 1);

        // Synchronize so we can ensure the worker is blocked before we try filling the queue.
        let started = Arc::new(Barrier::new(2));
        let release = Arc::new(Barrier::new(2));

        let started2 = started.clone();
        let release2 = release.clone();

        // Occupy the only worker.
        pool.try_submit(Box::new(move || {
            started2.wait();
            release2.wait();
        }))
        .unwrap();

        // Wait until the worker is definitely running the first job.
        started.wait();

        // Fill the queue with one job.
        let ran = Arc::new(AtomicBool::new(false));
        let ran2 = ran.clone();
        pool.try_submit(Box::new(move || {
            ran2.store(true, Ordering::Relaxed);
        }))
        .unwrap();

        // Next submission should fail.
        assert!(pool.try_submit(Box::new(|| {})).is_err());

        // Release the worker.
        release.wait();

        // The queued job should eventually run.
        for _ in 0..1000 {
            if ran.load(Ordering::Relaxed) {
                return;
            }
            std::thread::sleep(std::time::Duration::from_millis(1));
        }

        panic!("queued job did not run");
    }
}
