import { defineConfig } from "vite"
import { VitePWA } from 'vite-plugin-pwa'
import elmPlugin from "vite-plugin-elm"

export default defineConfig(({ mode }) => {
  const isDev = mode === 'development'
  return {
    base: isDev
      ? '/'
      : '/pop3-game/',
    build: {
      publicDir: 'public'
    },
    plugins: [
      VitePWA({
        includeAssets: ['favicon.ico'],
        manifest: {
          name: "Pop 3!",
          short_name: "pop3",
          display: "standalone",
          description: "A classic mini-game",
          background_color: '#333333',
          icons: [{
            src: "img/logo.png",
            sizes: "256x256",
            type: "image/png",
          }],
        }
      }),
      elmPlugin({
        debug: false,
      }),
    ],
  }
})
