// @ts-check
import { defineConfig } from 'astro/config';
import starlight from '@astrojs/starlight';

import ngn_grammar from './src/assets/ngn.tmLanguage.json' with { type: 'json' }
//const ngnGrammar = JSON.parse(fileURLToPath(new URL('./src/assets/ngn.tmLanguage.json', import.meta.url)))

// https://astro.build/config
export default defineConfig({
	redirects: {
		'/install': '/install.sh'
	},
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
					label: 'Start',
					items: [
						{ label: 'Overview', slug: 'start' },
						{ label: 'Install', slug: 'start/install' },
						{ label: 'Hello World', slug: 'start/hello-world' },
					],
				},
				{
					label: 'Learn',
					items: [
						{ label: 'WTF?', slug: 'learn/wtf' },
						{ label: 'Sick Picks', slug: 'learn/sick-picks' },
						{ label: 'Basics', slug: 'learn/basics' },
					],
				},
				{
					label: 'Build',
					items: [
						{ label: 'Web Server', slug: 'build/http' },
						{ label: 'MCP Server', slug: 'build/mcp' },
						{ label: 'LLM Inference', slug: 'build/llm' },
					],
				},
				{
					label: 'The Manual',
					autogenerate: { directory: 'the-manual' },
				},
			],
		}),
	],
});
