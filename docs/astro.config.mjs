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
					label: 'init',
					items: [
						{ label: 'Overview', slug: 'init' },
						{ label: 'Let\'s Go', slug: 'init/install' },
						{ label: 'Hello World', slug: 'init/hello-world' },
					],
				},
				{
					label: 'learn',
					items: [
						{ label: 'wtf?', slug: 'learn/wtf' },
						{ label: 'Sick Picks', slug: 'learn/sick-picks' },
						{ label: 'Basics', slug: 'learn/basics' },
						{ label: 'Language Tour', slug: 'learn/language-tour' },
						{ label: 'HTTP Servers', slug: 'learn/http-servers' },
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
