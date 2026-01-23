import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],

  build: {
    // Generate manifest for Kirin
    manifest: true,

    rollupOptions: {
      input: {
        main: 'index.html',
      },
    },
  },

  // SSR build configuration
  ssr: {
    // Entry for server-side rendering
    // Build with: vite build --ssr src/entry-server.tsx
  },

  server: {
    // Dev server for HMR
    port: 5173,
    strictPort: true,

    // Proxy API requests to Kirin during development
    proxy: {
      '/api': {
        target: 'http://localhost:3000',
        changeOrigin: true,
      },
    },
  },
});
