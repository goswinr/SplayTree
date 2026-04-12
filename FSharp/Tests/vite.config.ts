import { defineConfig } from "vite";
// import dts from "vite-plugin-dts";



export default defineConfig({
  // plugins: [
  //   dts({
  //     include: ["src/**/*"],
  //     exclude: ["tests/**/*", "demo/**/*", "bench/**/*"],
  //   }),
  // ],
  build: {
    minify: false,
    lib: {
      entry: "_js/SplayTree.js",
      name: "SplayTree",
    },
    sourcemap: true,
    rollupOptions: {
      input: "_js/SplayTree.js",
      output: [
        {
          format: "es"
        }
      ]
    }
  }
});
