import { bench, describe } from "vitest";

// import Splay from "../src/index"; // the original TS
import Splay from "../dist/splaytree"; // the original

import {
  Tree$2_$ctor_47C913C as createFSharpSplay,
  Tree$2__Find_2B595 as fsharpFind,
  Tree$2__FindStatic_2B595 as fsharpFindStatic,
  Tree$2__Insert_2B595 as fsharpInsert,
  Tree$2__Load_32EFB1E as fsharpLoad,
  Tree$2__Load_Z76B293CC as fsharpLoadWithPresort,
  Tree$2__Remove_2B595 as fsharpRemove,
  Tree$2__Update_5BDDA0 as fsharpUpdate,
// } from "../FSharp/Tests/_js/SplayTree.js"; // the fable build
} from "../FSharp/Tests/dist/splaytree.mjs"; // the fable build packed with vite

function generateRValues(N: number, min = 0, max = N): number[] {
  const map: Record<number, boolean> = {};
  const res: number[] = [];
  let i = 0;
  while (i < N) {
    const v = min + Math.floor(Math.random() * max);
    if (!map[v]) {
      map[v] = true;
      res.push(v);
      i++;
    }
  }
  return res;
}

const N = 1000;
const rvalues = generateRValues(N);
const values = new Array(N).fill(0).map((n, i) => i);

const comparator = (a: number, b: number): -1 | 0 | 1 => {
  if (a < b) return -1;
  if (a > b) return 1;
  return 0;
};

function createFSharpTree() {
  return createFSharpSplay(comparator);
}

function createPrefilledSplay() {
  const splay = new Splay(comparator);
  rvalues.forEach((v) => splay.insert(v));
  return splay;
}

function createPrefilledFSharpSplay() {
  const splay = createFSharpTree();
  rvalues.forEach((v) => fsharpInsert(splay, v));
  return splay;
}

const prefilledSplay = createPrefilledSplay();

const prefilledFSharpSplay = createPrefilledFSharpSplay();

describe(`F#<>TS: Insert (x${N})`, () => {
  bench("F#<>TS: Splay (current) - insert", () => {
    let splay = new Splay(comparator);
    for (let i = 0; i < N; i++) splay.insert(rvalues[i]);
  });

  bench("F#<>TS: Splay-FSharp (current) - insert", () => {
    const splay = createFSharpTree();
    for (let i = 0; i < N; i++) fsharpInsert(splay, rvalues[i]);
  });
});


describe(`F#<>TS: Random read (x${N})`, () => {
  bench("F#<>TS: Splay (current) - findStatic", () => {
    for (let i = N - 1; i; i--) prefilledSplay.findStatic(rvalues[i]);
  });

  bench("F#<>TS: Splay-FSharp (current) - findStatic", () => {
    for (let i = N - 1; i; i--)
      fsharpFindStatic(prefilledFSharpSplay, rvalues[i]);
  });
});

describe(`F#<>TS: Remove (x${N})`, () => {
  bench("F#<>TS: Splay (current) - remove", () => {
    const splay = createPrefilledSplay();
    for (let i = 0; i < N; i++) splay.remove(rvalues[i]);
  });

  bench("F#<>TS: Splay-FSharp (current) - remove", () => {
    const splay = createPrefilledFSharpSplay();
    for (let i = 0; i < N; i++) fsharpRemove(splay, rvalues[i]);
  });
});


const L = 1000;
const K = 1000;
const batch1 = new Array(L).fill(0).map((_, i) => i);
const batch2 = generateRValues(K, L);

describe(`F#<>TS: Splay: Bulk-add (x${K}) to ${L}`, () => {
  bench("F#<>TS: Splay: 1 by 1", () => {
    const t = new Splay();
    t.load(batch1);
    for (let i = 0; i < K; i++) t.insert(batch2[i]);
  });

  bench("F#<>TS: Splay-FSharp: 1 by 1", () => {
    const t = createFSharpTree();
    fsharpLoad(t, batch1);
    for (let i = 0; i < K; i++) fsharpInsert(t, batch2[i]);
  });
});

const G = 10000;
const P = 0.1;
const F = Math.round(G * P);
const data = generateRValues(G).sort(comparator);
const toUpdate = generateRValues(F, 0, G)
  .map((id) => data[id])
  .sort(comparator);

describe(`F#<>TS: Splay: Bulk-remove-insert (${P * 100}%) of ${G}`, () => {
  bench("F#<>TS: Splay: bulk add (rebuild)", () => {
    const t = new Splay();
    t.load(data);
    for (let i = 0; i < F; i++) {
      t.remove(toUpdate[i]);
    }
    t.load(toUpdate);
  });

  bench("F#<>TS: Splay-FSharp: bulk add (rebuild)", () => {
    const t = createFSharpTree();
    fsharpLoad(t, data);
    for (let i = 0; i < F; i++) {
      fsharpRemove(t, toUpdate[i]);
    }
    fsharpLoad(t, toUpdate);
  });
});

describe(`F#<>TS: Splay: Bulk-update (${P * 100}%) of ${G}`, () => {
  bench("F#<>TS: Splay: 1 by 1", () => {
    const t = new Splay();
    t.load(data);
    for (let i = 0; i < F; i++) {
      const offset = (i & 1 ? 1 : -1) * 5000;
      t.remove(toUpdate[i]);
      t.insert(toUpdate[i] + offset);
    }
  });

  bench("F#<>TS: Splay-FSharp: 1 by 1", () => {
    const t = createFSharpTree();
    fsharpLoad(t, data);
    for (let i = 0; i < F; i++) {
      const offset = (i & 1 ? 1 : -1) * 5000;
      fsharpRemove(t, toUpdate[i]);
      fsharpInsert(t, toUpdate[i] + offset);
    }
  });
});




