let tetrominoIndices = [];
let tetrominoPositions = [];
let tetrominoStart = 0;
let tetrominoEnd = 0;
let cubeTranslations = [];
let tetrominoCubes = [];

var inputTriangles = [
  {
    "material": {
      "ambient": [
        1.0,
        1.0,
        1.0
      ],
      "diffuse": [
        1.0,
        1.0,
        1.0
      ],
      "specular": [
        1.0,
        1.0,
        1.0
      ],
      "texture": "faithfultetrisbak.gif",
      "n": 17
    },
    "vertices": [
      [
        -0.70,
        -0.70,
        0.75
      ],
      [
        -0.70,
        1.70,
        0.75
      ],
      [
        1.70,
        1.70,
        0.75
      ],
      [
        1.70,
        -0.70,
        0.75
      ]
    ],
    "normals": [
      [
        0,
        0,
        -1
      ],
      [
        0,
        0,
        -1
      ],
      [
        0,
        0,
        -1
      ],
      [
        0,
        0,
        -1
      ]
    ],
    "triangles": [
      [
        0,
        1,
        2
      ],
      [
        2,
        3,
        0
      ]
    ],
    "uvs": [[0, 0], [0, 1], [1, 1], [1, 0]],
  },
];

function generateCube(texture) {
  return {
    "material": {
      "ambient": [
        1.0,
        1.0,
        1.0
      ],
      "diffuse": [
        1.0,
        1.0,
        1.0
      ],
      "specular": [
        1.0,
        1.0,
        1.0
      ],
      "texture": texture,
      "n": 17
    },
    "vertices": [
      // FRONT
      [
        -0.70,
        -0.70,
        0.75
      ],
      [
        -0.70,
        1.70,
        0.75
      ],
      [
        1.70,
        1.70,
        0.75
      ],
      [
        1.70,
        -0.70,
        0.75
      ],

      // BACK
      [
        -0.70,
        -0.70,
        0.75 + (1.70 + 0.70)
      ],
      [
        -0.70,
        1.70,
        0.75 + (1.70 + 0.70)
      ],
      [
        1.70,
        1.70,
        0.75 + (1.70 + 0.70)
      ],
      [
        1.70,
        -0.70,
        0.75 + (1.70 + 0.70)
      ],
      // TOP
      [
        -0.70,
        1.70,
        0.75
      ],
      [
        -0.70,
        1.70,
        0.75 + (1.70 + 0.70)
      ],
      [
        1.70,
        1.70,
        0.75 + (1.70 + 0.70)
      ],
      [
        1.70,
        1.70,
        0.75
      ],
      // BOTTOM
      [
        -0.70,
        -0.70,
        0.75
      ],
      [
        -0.70,
        -0.70,
        0.75 + (1.70 + 0.70)
      ],
      [
        1.70,
        -0.70,
        0.75 + (1.70 + 0.70)
      ],
      [
        1.70,
        -0.70,
        0.75
      ],
      // RIGHT
      [
        -0.70,
        -0.70,
        0.75
      ],
      [
        -0.70,
        -0.70,
        0.75 + (1.70 + 0.70)
      ],
      [
        -0.70,
        1.70,
        0.75 + (1.70 + 0.70)
      ],
      [
        -0.70,
        1.70,
        0.75
      ],
      // LEFT
      [
        1.70,
        -0.70,
        0.75
      ],
      [
        1.70,
        -0.70,
        0.75 + (1.70 + 0.70)
      ],
      [
        1.70,
        1.70,
        0.75 + (1.70 + 0.70)
      ],
      [
        1.70,
        1.70,
        0.75
      ],
    ],
    "normals": [
      //FRONT
      [
        0,
        0,
        -1
      ],
      [
        0,
        0,
        -1
      ],
      [
        0,
        0,
        -1
      ],
      [
        0,
        0,
        -1
      ],
      // BACK
      [
        0,
        0,
        1
      ],
      [
        0,
        0,
        1
      ],
      [
        0,
        0,
        1
      ],
      [
        0,
        0,
        1
      ],
      // TOP
      [
        0,
        1,
        0
      ],
      [
        0,
        1,
        0
      ],
      [
        0,
        1,
        0
      ],
      [
        0,
        1,
        0
      ],
      // BOTTOM
      [
        0,
        -1,
        0
      ],
      [
        0,
        -1,
        0
      ],
      [
        0,
        -1,
        0
      ],
      [
        0,
        -1,
        0
      ],
      // RIGHT
      [
        1,
        0,
        0
      ],
      [
        1,
        0,
        0
      ],
      [
        1,
        0,
        0
      ],
      [
        1,
        0,
        0
      ],
      // LEFT
      [
        -1,
        0,
        0
      ],
      [
        -1,
        0,
        0
      ],
      [
        -1,
        0,
        0
      ],
      [
        -1,
        0,
        0
      ],
    ],
    "triangles": [
      // FRONT
      [
        0,
        1,
        2
      ],
      [
        2,
        3,
        0
      ],
      // BACK
      [
        0 + 4 * 1,
        1 + 4 * 1,
        2 + 4 * 1
      ],
      [
        2 + 4 * 1,
        3 + 4 * 1,
        0 + 4 * 1
      ],
      // TOP
      [
        0 + 4 * 2,
        1 + 4 * 2,
        2 + 4 * 2
      ],
      [
        2 + 4 * 2,
        3 + 4 * 2,
        0 + 4 * 2
      ],
      // BOTTOM
      [
        0 + 4 * 3,
        1 + 4 * 3,
        2 + 4 * 3
      ],
      [
        2 + 4 * 3,
        3 + 4 * 3,
        0 + 4 * 3
      ],
      // RIGHT
      [
        0 + 4 * 4,
        1 + 4 * 4,
        2 + 4 * 4
      ],
      [
        2 + 4 * 4,
        3 + 4 * 4,
        0 + 4 * 4
      ],
      // LEFT
      [
        0 + 4 * 5,
        1 + 4 * 5,
        2 + 4 * 5
      ],
      [
        2 + 4 * 5,
        3 + 4 * 5,
        0 + 4 * 5
      ]
    ],
    "uvs": [
      // FRONT
      [0, 0], [0, 1], [1, 1], [1, 0],
      // BACK
      [0, 1], [0, 0], [1, 0], [1, 1],
      // TOP
      [0, 0], [0, 1], [1, 1], [1, 0],
      // BOTTOM
      [0, 1], [0, 0], [1, 0], [1, 1],
      // RIGHT
      [1, 0], [0, 0], [0, 1], [1, 1],
      // LEFT
      [1, 0], [0, 0], [0, 1], [1, 1]
    ],
  };
}

function generateTetrominoPiece(texture) {
  const cube = generateCube(texture);
  const cubeScale = mat4.create(1);
  mat4.fromScaling(cubeScale, vec3.fromValues(0.0130, 0.0130, 0.0130));
  let newVertices = [];
  for (const vtx of cube.vertices) {
    const newPtVec = vec3.fromValues(...vtx);
    vec3.transformMat4(newPtVec, newPtVec, cubeScale);
    newVertices.push(Object.values(newPtVec));
  }
  cube.vertices = newVertices;

  const inverseTranspose = mat4.create(1);
  mat4.invert(inverseTranspose, cubeScale);
  mat4.transpose(inverseTranspose, inverseTranspose);
  let newNormals = [];
  for (const vtx of cube.normals) {
    const newPtVec = vec3.fromValues(...vtx);
    vec3.transformMat4(newPtVec, newPtVec, inverseTranspose);
    newNormals.push(Object.values(newPtVec));
    vec3.fromValues(...newPtVec);
  }
  cube.normals = newNormals;
  return cube;
}

// THE PEICES:
// 0, 0 IS ON THE LEFT!
// 0, 0 CORRESPONDS TO THE BOTTOM LEFT! CORNER
//
// 0: T, tetrominotex0.gif
//
//  XXX
//   X
//
let tRot0 =
  [
    [1, 1, 1],
    [0, 1, 0],
  ];
let tRot90 =
  [
    [1, 0],
    [1, 1],
    [1, 0],
  ];
let tRot180 =
  [
    [0, 1, 0],
    [1, 1, 1],
  ];
let tRot270 =
  [
    [0, 1],
    [1, 1],
    [0, 1],
  ];

let tRot = [tRot0, tRot90, tRot180, tRot270];
// 1: L, tetrominotex1.gif
//
//  XXX
//    X
//
let lRot0 = [
  [1, 1, 1],
  [0, 0, 1]
];

let lRot90 = [
  [0, 1],
  [0, 1],
  [1, 1]
];

let lRot180 = [
  [0, 0, 1],
  [1, 1, 1]
];

let lRot270 = [
  [1, 0],
  [1, 0],
  [1, 1]
];

let lRot = [lRot0, lRot90, lRot180, lRot270];
// 2: Z, tetrominotex2.gif
//
//  XX
//   XX
//
//   X
//  XX
//  X
let zRot0 = [
  [1, 1, 0],
  [0, 1, 1]
];

let zRot90 = [
  [0, 1],
  [1, 1],
  [1, 0]
];

let zRot180 = [
  [0, 1, 1],
  [1, 1, 0]
];

let zRot270 = [
  [1, 0],
  [1, 1],
  [0, 1]
];

let zRot = [zRot0, zRot90, zRot180, zRot270];

// 3: O, tetrominotex3.gif
//
//  XX
//  XX
//
let oRot0 = [
  [1, 1],
  [1, 1]
];

let oRot = [oRot0, oRot0, oRot0, oRot0];
// 4: S, tetrominotex4.gif
//
//   XX
//  XX
//
let sRot0 = [
  [0, 1, 1],
  [1, 1, 0]
];

let sRot90 = [
  [1, 0],
  [1, 1],
  [0, 1]
];
let sRot180 = [
  [1, 1, 0],
  [0, 1, 1]
];

let sRot270 = [
  [0, 1],
  [1, 1],
  [1, 0]
];

let sRot = [sRot0, sRot90, sRot180, sRot270];
// 5: J, tetrominotex5.gif
//
//  XXX
//  X
//
let jRot0 = [
  [1, 1, 1],
  [1, 0, 0]
];

let jRot90 = [
  [1, 0],
  [1, 0],
  [1, 1]
];

let jRot180 = [
  [0, 0, 1],
  [1, 1, 1]
];

let jRot270 = [
  [0, 1],
  [0, 1],
  [1, 1]
];

let jRot = [jRot0, jRot90, jRot180, jRot270];
// 6: I, tetrominotex6.gif
//
//  XXXX
let iRot0 = [
  [1, 1, 1, 1],
  [0, 0, 0, 0]
];

let iRot90 = [
  [1, 0],
  [1, 0],
  [1, 0],
  [1, 0]
];

let iRot180 = [
  [1, 1, 1, 1],
  [0, 0, 0, 0]
];

let iRot270 = [
  [1, 0],
  [1, 0],
  [1, 0],
  [1, 0]
];

let iRot = [iRot0, iRot90, iRot180, iRot270];

function placeTetrominoPiece(i, j, tetrominoPiece) {
  {
    tetrominoPiece.cubeTranslation = vec3.fromValues(i * 0.030 + 0.30, j * 0.03 + 0.12, 0);
    tetrominoPiece.cubeDisp = i;
    tetrominoPiece.cubeDepth = 23 - j;
  }

}

function makeShape(tex, rot) {
  let indices = [];
  let posns = [];
  let cubes = [];
  for (let k = 0; k < 4; k++) {
    let trianglesBegin = 0;
    let trianglesEnd = 0;
    let beg = true;
    let rotposns = [];
    cubes[k] = [];
    for (let i = 0; i < rot[k].length; i++) {
      cubes[k][i] = [];
      for (let j = 0; j < rot[k][i].length; j++) {
        cubes[k][i][j] = [];
        if (rot[k][i][j] === 1) {
          cubes[k][i][j][0] = inputTriangles.length;
          const tetrominoPiece = generateTetrominoPiece(tex);
          trianglesEnd = inputTriangles.push(tetrominoPiece) - 1;
          placeTetrominoPiece(10 - j, 23 - i, tetrominoPiece);
          cubes[k][i][j][1] = trianglesEnd;
          rotposns.push([j, 23 - i]);
          if (beg) {
            trianglesBegin = trianglesEnd;
            beg = false;
          }
        } else {
          cubes[k][i][j][0] = -1;
          cubes[k][i][j][1] = -1;
        }
      }
    }
    posns.push(rotposns);
    indices.push([trianglesBegin, trianglesEnd]);
  }
  tetrominoIndices.push(indices);
  tetrominoPositions.push(posns);
  tetrominoCubes.push(cubes);
}

// tetromino PROTOTYPE generators
const rangeT = 7;
function makeT() {
  makeShape("tetrominotex0.gif", tRot);
}

const rangeL = 6;
function makeL() {
  makeShape("tetrominotex1.gif", lRot);
}

const rangeZ = 7;
function makeZ() {
  makeShape("tetrominotex2.gif", zRot);
}

const rangeO = 8;
function makeO() {
  makeShape("tetrominotex3.gif", oRot);
}

const rangeS = 7;
function makeS() {
  makeShape("tetrominotex4.gif", sRot);
}

const rangeJ = 7;
function makeJ() {
  makeShape("tetrominotex5.gif", jRot);
}

const rangeI = 6;
function makeI() {
  makeShape("tetrominotex6.gif", iRot);
}

// Make all ye tetromino prototypes
let makeTetrominos = [
  makeT,
  makeL,
  makeZ,
  makeO,
  makeS,
  makeJ,
  makeI,
];

let rangeTetrominos = [
  rangeT,
  rangeL,
  rangeZ,
  rangeO,
  rangeS,
  rangeJ,
  rangeI
];

// Load input tetrominos
tetrominoStart = inputTriangles.length;
for (const make of makeTetrominos) {
  make();
}
tetrominoEnd = inputTriangles.length;
