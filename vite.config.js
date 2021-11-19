import { defineConfig } from "vite"
import { VitePWA } from 'vite-plugin-pwa'
import elmPlugin from "vite-plugin-elm"

export default defineConfig(({ mode }) => {
  const isDev = mode === 'development'
  return {
    base: isDev
      ? '/'
      : '/pop3-game/',
    plugins: [
      VitePWA({
        registerType: 'autoUpdate',
        includeAssets: ['favicon.ico'],
        manifest: {
          name: "Pop 3!",
          short_name: "pop3",
          display: "standalone",
          description: "A classic mini-game",
          theme_color: '#d06060',
          background_color: '#222222',
          icons: [{
            src: "img/pop3_icon.png",
            sizes: "512x512",
            type: "image/png",
          }, {
            src: "img/pop3_maskable.png",
            sizes: "640x640",
            type: "image/png",
            purpose: "maskable",
          }],
        }
      }),
      elmPlugin({
        debug: false,
      }),
    ],
  }
})
