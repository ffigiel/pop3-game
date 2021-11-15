import { defineConfig } from "vite"
import elmPlugin from "vite-plugin-elm"

export default defineConfig({
  build: {
    publicDir: 'public'
  },
  plugins: [
    elmPlugin({ debug: false }),
  ],
})
