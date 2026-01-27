import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig({
  plugins: [
    react({
      include: ["**/*.res.js"],
    }),
  ],
  server: {
    proxy: {
      "/graphql": "http://localhost:9000",
    },
  },
});
