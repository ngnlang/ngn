// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

import ngn_grammar from './src/assets/ngn.tmLanguage.json' with { type: 'json' }
//const ngnGrammar = JSON.parse(fileURLToPath(new URL('./src/assets/ngn.tmLanguage.json', import.meta.url)))

// https://astro.build/config
export default defineConfig({
	markdown: {
		shikiConfig: {
			langs: [
				{
					name: 'ngn',
					...ngn_grammar
				},
			],
		},
	},
	integrations: [
		starlight({
			title: 'ngn',
			logo: {
				light: './src/assets/ngn-light-512.png',
				dark: './src/assets/ngn-dark-512.png',
				alt: 'ngn',
			},
			sidebar: [
				{
					label: 'Getting Started',
					items: [
						{ label: 'Overview', slug: 'getting-started' },
						{ label: 'Install', slug: 'getting-started/install' },
						{ label: 'Hello World', slug: 'getting-started/hello-world' },
						{ label: 'Next Steps', slug: 'getting-started/next-steps' },
					],
				},
				{
					label: 'Guides',
					items: [
						// Each item here is one entry in the navigation menu.
						{ label: 'Language Tour', slug: 'guides/language-tour' },
						{ label: 'Concurrency Basics', slug: 'guides/concurrency-basics' },
						{ label: 'HTTP Servers', slug: 'guides/http-servers' },
					],
				},
				{
					label: 'Reference',
					autogenerate: { directory: 'reference' },
				},
			],
		}),
	],
});
