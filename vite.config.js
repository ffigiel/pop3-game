import { defineConfig } from "vite"
import { VitePWA } from 'vite-plugin-pwa'
import elmPlugin from "vite-plugin-elm"

export default defineConfig({
  plugins: [
    VitePWA({
      registerType: 'autoUpdate',
      includeAssets: ['favicon.ico'],
      manifest: {
        name: "Pop 3",
        short_name: "Pop 3",
        display: "standalone",
        description: "A classic mini-game",
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
})
