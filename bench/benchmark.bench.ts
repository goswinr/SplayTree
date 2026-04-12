import { bench, describe } from "vitest";
import { AVLTree } from "avl";
import { RBTree } from "bintrees";
import Splay from "../src/index";
import {
  Tree$2_$ctor_47C913C as createFSharpSplay,
  Tree$2__Find_2B595 as fsharpFind,
  Tree$2__FindStatic_2B595 as fsharpFindStatic,
  Tree$2__Insert_2B595 as fsharpInsert,
  Tree$2__Load_32EFB1E as fsharpLoad,
  Tree$2__Load_Z76B293CC as fsharpLoadWithPresort,
  Tree$2__Remove_2B595 as fsharpRemove,
  Tree$2__Update_5BDDA0 as fsharpUpdate,
} from "../FSharp/Tests/_js/SplayTree.js";

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

const comparator = (a: number, b: number) => a - b;

function createFSharpTree() {
  return createFSharpSplay(comparator);
}

const prefilledAVL = new AVLTree();
rvalues.forEach((v) => prefilledAVL.insert(v));

const prefilledRB = new RBTree(comparator);
rvalues.forEach((v) => prefilledRB.insert(v));

const prefilledSplay = new Splay(comparator);
rvalues.forEach((v) => prefilledSplay.insert(v));

const prefilledFSharpSplay = createFSharpTree();
rvalues.forEach((v) => fsharpInsert(prefilledFSharpSplay, v));

describe(`Insert (x${N})`, () => {
  bench("Bintrees RB", () => {
    let rb = new RBTree(comparator);
    for (let i = 0; i < N; i++) rb.insert(rvalues[i]);
  });

  bench("Splay (current)", () => {
    let splay = new Splay(comparator);
    for (let i = 0; i < N; i++) splay.insert(rvalues[i]);
  });

  bench("Splay-FSharp (current)", () => {
    const splay = createFSharpTree();
    for (let i = 0; i < N; i++) fsharpInsert(splay, rvalues[i]);
  });

  bench("AVL", () => {
    const tree = new AVLTree();
    for (let i = 0; i < N; i++) tree.insert(rvalues[i]);
  });
});

describe(`Random read (x${N})`, () => {
  bench("Bintrees RB", () => {
    for (let i = N - 1; i; i--) prefilledRB.find(rvalues[i]);
  });

  bench("Splay (current) - find", () => {
    for (let i = N - 1; i; i--) prefilledSplay.find(rvalues[i]);
  });

  bench("Splay-FSharp (current) - find", () => {
    for (let i = N - 1; i; i--) fsharpFind(prefilledFSharpSplay, rvalues[i]);
  });

  bench("Splay (current) - findStatic", () => {
    for (let i = N - 1; i; i--) prefilledSplay.findStatic(rvalues[i]);
  });

  bench("Splay-FSharp (current) - findStatic", () => {
    for (let i = N - 1; i; i--) fsharpFindStatic(prefilledFSharpSplay, rvalues[i]);
  });

  bench("AVL", () => {
    for (let i = N - 1; i; i--) prefilledAVL.find(rvalues[i]);
  });
});

describe(`Remove (x${N})`, () => {
  bench("Bintrees RB", () => {
    for (let i = 0; i < N; i++) prefilledRB.remove(rvalues[i]);
  });

  bench("Splay (current)", () => {
    for (let i = 0; i < N; i++) prefilledSplay.remove(rvalues[i]);
  });

  bench("Splay-FSharp (current)", () => {
    for (let i = 0; i < N; i++) fsharpRemove(prefilledFSharpSplay, rvalues[i]);
  });

  bench("AVL", () => {
    for (let i = N - 1; i; i--) prefilledAVL.remove(values[i]);
  });
});

const M = 10000;
const arr = generateRValues(M);

describe(`Splay: Bulk-load (x${M})`, () => {
  bench("Splay: 1 by 1", () => {
    const t = new Splay();
    for (let i = 0; i < M; i++) t.insert(arr[i]);
  });

  bench("Splay-FSharp: 1 by 1", () => {
    const t = createFSharpTree();
    for (let i = 0; i < M; i++) fsharpInsert(t, arr[i]);
  });

  bench("Splay: bulk load (build)", () => {
    const t = new Splay();
    const data = arr.slice();
    t.load(data, [], true);
  });

  bench("Splay-FSharp: bulk load (build)", () => {
    const t = createFSharpTree();
    const data = arr.slice();
    fsharpLoadWithPresort(t, data, true);
  });
});

const L = 1000;
const K = 1000;
const batch1 = new Array(L).fill(0).map((_, i) => i);
const batch2 = generateRValues(K, L);

describe(`Splay: Bulk-add (x${K}) to ${L}`, () => {
  bench("Splay: 1 by 1", () => {
    const t = new Splay();
    t.load(batch1);
    for (let i = 0; i < K; i++) t.insert(batch2[i]);
  });

  bench("Splay-FSharp: 1 by 1", () => {
    const t = createFSharpTree();
    fsharpLoad(t, batch1);
    for (let i = 0; i < K; i++) fsharpInsert(t, batch2[i]);
  });

  bench("Splay: bulk add (rebuild)", () => {
    const t = new Splay();
    t.load(batch1);
    t.load(batch2);
  });

  bench("Splay-FSharp: bulk add (rebuild)", () => {
    const t = createFSharpTree();
    fsharpLoad(t, batch1);
    fsharpLoad(t, batch2);
  });
});

const G = 10000;
const P = 0.1;
const F = Math.round(G * P);
const data = generateRValues(G).sort(comparator);
const toUpdate = generateRValues(F, 0, G)
  .map((id) => data[id])
  .sort(comparator);

describe(`Splay: Bulk-remove-insert (${P * 100}%) of ${G}`, () => {
  bench("1 by 1", () => {
    const t = new Splay();
    t.load(data);
    for (let i = 0; i < F; i++) {
      t.remove(toUpdate[i]);
      t.insert(toUpdate[i]);
    }
  });

  bench("Splay: bulk add (rebuild)", () => {
    const t = new Splay();
    t.load(data);
    for (let i = 0; i < F; i++) {
      t.remove(toUpdate[i]);
    }
    t.load(toUpdate);
  });

  bench("Splay-FSharp: bulk add (rebuild)", () => {
    const t = createFSharpTree();
    fsharpLoad(t, data);
    for (let i = 0; i < F; i++) {
      fsharpRemove(t, toUpdate[i]);
    }
    fsharpLoad(t, toUpdate);
  });
});

describe(`Splay: Bulk-update (${P * 100}%) of ${G}`, () => {
  bench("Splay: 1 by 1", () => {
    const t = new Splay();
    t.load(data);
    for (let i = 0; i < F; i++) {
      const offset = (i & 1 ? 1 : -1) * 5000;
      t.remove(toUpdate[i]);
      t.insert(toUpdate[i] + offset);
    }
  });

  bench("Splay-FSharp: 1 by 1", () => {
    const t = createFSharpTree();
    fsharpLoad(t, data);
    for (let i = 0; i < F; i++) {
      const offset = (i & 1 ? 1 : -1) * 5000;
      fsharpRemove(t, toUpdate[i]);
      fsharpInsert(t, toUpdate[i] + offset);
    }
  });

  bench("Splay: split-merge", () => {
    const t = new Splay();
    t.load(data);
    for (let i = 0; i < F; i++) {
      const offset = (i & 1 ? 1 : -1) * 5000;
      t.update(toUpdate[i], toUpdate[i] + offset);
    }
  });

  bench("Splay-FSharp: split-merge", () => {
    const t = createFSharpTree();
    fsharpLoad(t, data);
    for (let i = 0; i < F; i++) {
      const offset = (i & 1 ? 1 : -1) * 5000;
      fsharpUpdate(t, toUpdate[i], toUpdate[i] + offset);
    }
  });
});
