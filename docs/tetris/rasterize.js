/* GLOBAL CONSTANTS AND VARIABLES */

/* assignment specific globals */
const INPUT_TRIANGLES_URL = "https://ncsucgclass.github.io/prog4/triangles.json"; // triangles file loc
var defaultEye = vec3.fromValues(0.5, 0.5, -0.5); // default eye position in world space
var defaultCenter = vec3.fromValues(0.5, 0.5, 0.5); // default view direction in world space
var defaultUp = vec3.fromValues(0, 1, 0); // default view up vector
var lightAmbient = vec3.fromValues(1, 1, 1); // default light ambient emission
var lightDiffuse = vec3.fromValues(1, 1, 1); // default light diffuse emission
var lightSpecular = vec3.fromValues(1, 1, 1); // default light specular emission
var lightPosition = vec3.fromValues(-0.5, 1.5, -0.5); // default light position
var rotateTheta = Math.PI / 50; // how much to rotate models by with each key press

/* webgl and geometry data */
var gl = null; // the all powerful gl object. It's all here folks!
var numTriangleSets = 0; // how many triangle sets in input scene
var vertexBuffers = []; // this contains vertex coordinate lists by set, in triples
var normalBuffers = []; // this contains normal component lists by set, in triples
var uvBuffers = [];
var triSetSizes = []; // this contains the size of each triangle set
var triangleBuffers = []; // lists of indices into vertexBuffers by set, in triples
var viewDelta = 0.1; // how much to displace view with each key press
var allCubes = [];

var tetMovedRight = false;
var tetMovedLeft = false;
var rotatedCC = false;
var tetMovedDown = false;

var nameToTex = {};
var setToName = [];
var setToTransparent = [];
var blendMode = 0.0;

var showcubes = true;

var tetrominoList = [];

/* shader parameter locations */
var vPosAttribLoc; // where to put position for vertex shader
var vNormAttribLoc;
var mMatrixULoc; // where to put model matrix for vertex shader
var pvmMatrixULoc; // where to put project model view matrix for vertex shader
var ambientULoc; // where to put ambient reflecivity for fragment shader
var diffuseULoc; // where to put diffuse reflecivity for fragment shader
var specularULoc; // where to put specular reflecivity for fragment shader
var shininessULoc; // where to put specular exponent for fragment shader
var samplerULoc;
var modeULoc;
var imageCanvas; // TODO fix
var uvAttribLoc;
var spacePressed;

/* interaction variables */
var Eye = vec3.clone(defaultEye); // eye position in world space
var Center = vec3.clone(defaultCenter); // view direction in world space
var Up = vec3.clone(defaultUp); // view up vector in world space

// ASSIGNMENT HELPER FUNCTIONS

// get the JSON file from the passed URL
function getJSONFile(url, descr) {
  try {
    if ((typeof (url) !== "string") || (typeof (descr) !== "string"))
      throw "getJSONFile: parameter not a string";
    else {
      var httpReq = new XMLHttpRequest(); // a new http request
      httpReq.open("GET", url, false); // init the request
      httpReq.send(null); // send the request
      var startTime = Date.now();
      while ((httpReq.status !== 200) && (httpReq.readyState !== XMLHttpRequest.DONE)) {
        if ((Date.now() - startTime) > 3000)
          break;
      } // until its loaded or we time out after three seconds
      if ((httpReq.status !== 200) || (httpReq.readyState !== XMLHttpRequest.DONE))
        throw "Unable to open " + descr + " file!";
      else
        return JSON.parse(httpReq.response);
    } // end if good params
  } // end try

  catch (e) {
    console.log(e);
    return (String.null);
  }
} // end get input json file

// does stuff when keys are pressed
function handleKeyDown(event) {

  const modelEnum = { TRIANGLES: "triangles", ELLIPSOID: "ellipsoid" }; // enumerated model type
  const dirEnum = { NEGATIVE: -1, POSITIVE: 1 }; // enumerated rotation direction

  function highlightModel(modelType, whichModel) {
    if (handleKeyDown.modelOn != null)
      handleKeyDown.modelOn.on = false;
    handleKeyDown.whichOn = whichModel;
    if (modelType == modelEnum.TRIANGLES)
      handleKeyDown.modelOn = inputTriangles[whichModel];
    else
      handleKeyDown.modelOn = inputEllipsoids[whichModel];
    handleKeyDown.modelOn.on = true;
  } // end highlight model

  function translateModel(offset) {
    if (handleKeyDown.modelOn != null)
      vec3.add(handleKeyDown.modelOn.translation, handleKeyDown.modelOn.translation, offset);
  } // end translate model

  function rotateModel(axis, direction) {
    if (handleKeyDown.modelOn != null) {
      var newRotation = mat4.create();

      mat4.fromRotation(newRotation, direction * rotateTheta, axis); // get a rotation matrix around passed axis
      vec3.transformMat4(handleKeyDown.modelOn.xAxis, handleKeyDown.modelOn.xAxis, newRotation); // rotate model x axis tip
      vec3.transformMat4(handleKeyDown.modelOn.yAxis, handleKeyDown.modelOn.yAxis, newRotation); // rotate model y axis tip
    } // end if there is a highlighted model
  } // end rotate model

  // set up needed view params
  var lookAt = vec3.create();
  var viewRight = vec3.create();
  var temp = vec3.create(); // lookat, right & temp vectors
  lookAt = vec3.normalize(lookAt, vec3.subtract(temp, Center, Eye)); // get lookat vector
  viewRight = vec3.normalize(viewRight, vec3.cross(temp, lookAt, Up)); // get view right vector

  // highlight static variables
  handleKeyDown.whichOn = handleKeyDown.whichOn == undefined ? -1 : handleKeyDown.whichOn; // nothing selected initially
  handleKeyDown.modelOn = handleKeyDown.modelOn == undefined ? null : handleKeyDown.modelOn; // nothing selected initially

  vec3.set(temp, 1, 1, 1);
  switch (event.code) {

    // model selection
    case "Space":
      // if (handleKeyDown.modelOn != null)
      //   handleKeyDown.modelOn.on = false; // turn off highlighted model
      // handleKeyDown.modelOn = null; // no highlighted model
      // handleKeyDown.whichOn = -1; // nothing highlighted
      showcubes = !showcubes;
      spacePressed = true;
      break;
    case "ArrowRight": // select next triangle set
      tetMovedRight = true;
      break;
    case "ArrowLeft": // select previous triangle set
      tetMovedLeft = true;
      break;
    case "ArrowUp": // Rotate counter clockwise
      rotatedCC = true;
      break;
    case "ArrowDown":
      tetMovedDown = true;
      break;

    // view change
    case "KeyA": // translate view left, rotate left with shift
      Center = vec3.add(Center, Center, vec3.scale(temp, viewRight, viewDelta));
      if (!event.getModifierState("Shift"))
        Eye = vec3.add(Eye, Eye, vec3.scale(temp, viewRight, viewDelta));
      break;
    case "KeyB":
      {
        blendMode += 1;
        blendMode %= 3;
        gl.uniform1f(modeULoc, blendMode); // pass in the mode value
        break;
      }
    case "KeyD": // translate view right, rotate right with shift
      Center = vec3.add(Center, Center, vec3.scale(temp, viewRight, -viewDelta));
      if (!event.getModifierState("Shift"))
        Eye = vec3.add(Eye, Eye, vec3.scale(temp, viewRight, -viewDelta));
      break;
    case "KeyS": // translate view backward, rotate up with shift
      if (event.getModifierState("Shift")) {
        Center = vec3.add(Center, Center, vec3.scale(temp, Up, viewDelta));
        Up = vec3.cross(Up, viewRight, vec3.subtract(lookAt, Center, Eye)); /* global side effect */
      } else {
        Eye = vec3.add(Eye, Eye, vec3.scale(temp, lookAt, -viewDelta));
        Center = vec3.add(Center, Center, vec3.scale(temp, lookAt, -viewDelta));
      } // end if shift not pressed
      break;
    case "KeyW": // translate view forward, rotate down with shift
      if (event.getModifierState("Shift")) {
        Center = vec3.add(Center, Center, vec3.scale(temp, Up, -viewDelta));
        Up = vec3.cross(Up, viewRight, vec3.subtract(lookAt, Center, Eye)); /* global side effect */
      } else {
        vec3.add(Eye, Eye, vec3.scale(temp, lookAt, viewDelta));
        vec3.add(Center, Center, vec3.scale(temp, lookAt, viewDelta));
      } // end if shift not pressed
      break;
    case "KeyQ": // translate view up, rotate counterclockwise with shift
      if (event.getModifierState("Shift"))
        Up = vec3.normalize(Up, vec3.add(Up, Up, vec3.scale(temp, viewRight, -viewDelta)));
      else {
        Eye = vec3.add(Eye, Eye, vec3.scale(temp, Up, viewDelta));
        Center = vec3.add(Center, Center, vec3.scale(temp, Up, viewDelta));
      } // end if shift not pressed
      break;
    case "KeyE": // translate view down, rotate clockwise with shift
      if (event.getModifierState("Shift"))
        Up = vec3.normalize(Up, vec3.add(Up, Up, vec3.scale(temp, viewRight, viewDelta)));
      else {
        Eye = vec3.add(Eye, Eye, vec3.scale(temp, Up, -viewDelta));
        Center = vec3.add(Center, Center, vec3.scale(temp, Up, -viewDelta));
      } // end if shift not pressed
      break;
    case "Escape": // reset view to default
      Eye = vec3.copy(Eye, defaultEye);
      Center = vec3.copy(Center, defaultCenter);
      Up = vec3.copy(Up, defaultUp);
      break;

    // model transformation
    case "KeyK": // translate left, rotate left with shift
      if (event.getModifierState("Shift"))
        rotateModel(Up, dirEnum.NEGATIVE);
      else
        translateModel(vec3.scale(temp, viewRight, viewDelta));
      break;
    case "Semicolon": // translate right, rotate right with shift
      if (event.getModifierState("Shift"))
        rotateModel(Up, dirEnum.POSITIVE);
      else
        translateModel(vec3.scale(temp, viewRight, -viewDelta));
      break;
    case "KeyL": // translate backward, rotate up with shift
      if (event.getModifierState("Shift"))
        rotateModel(viewRight, dirEnum.POSITIVE);
      else
        translateModel(vec3.scale(temp, lookAt, -viewDelta));
      break;
    case "KeyO": // translate forward, rotate down with shift
      if (event.getModifierState("Shift"))
        rotateModel(viewRight, dirEnum.NEGATIVE);
      else
        translateModel(vec3.scale(temp, lookAt, viewDelta));
      break;
    case "KeyI": // translate up, rotate counterclockwise with shift
      if (event.getModifierState("Shift"))
        rotateModel(lookAt, dirEnum.POSITIVE);
      else
        translateModel(vec3.scale(temp, Up, viewDelta));
      break;
    case "KeyP": // translate down, rotate clockwise with shift
      if (event.getModifierState("Shift"))
        rotateModel(lookAt, dirEnum.NEGATIVE);
      else
        translateModel(vec3.scale(temp, Up, -viewDelta));
      break;
    case "Backspace": // reset model transforms to default
      for (var whichTriSet = 0; whichTriSet < numTriangleSets; whichTriSet++) {
        vec3.set(inputTriangles[whichTriSet].translation, 0, 0, 0);
        vec3.set(inputTriangles[whichTriSet].xAxis, 1, 0, 0);
        vec3.set(inputTriangles[whichTriSet].yAxis, 0, 1, 0);
      } // end for all triangle sets
      for (var whichEllipsoid = 0; whichEllipsoid < numEllipsoids; whichEllipsoid++) {
        vec3.set(inputEllipsoids[whichEllipsoid].translation, 0, 0, 0);
        vec3.set(inputEllipsoids[whichTriSet].xAxis, 1, 0, 0);
        vec3.set(inputEllipsoids[whichTriSet].yAxis, 0, 1, 0);
      } // end for all ellipsoids
      break;
  } // end switch
} // end handleKeyDown

// Stackoverflow function for euclidean modulo
// https://stackoverflow.com/questions/11720656/modulo-operation-with-negative-numbers
function modulo_Euclidean(a, b) {
  let m = a % b;
  if (m < 0) {
    // m += (b < 0) ? -b : b; // avoid this form: it is UB when b == INT_MIN
    m = (b < 0) ? m - b : m + b;
  }
  return m;
}

// set up the webGL environment
function setupWebGL() {

  // Set up keys
  document.onkeydown = handleKeyDown; // call this when key pressed

  // Get the canvas and context
  var canvas = document.getElementById("myWebGLCanvas"); // create a js canvas
  imageCanvas = canvas;
  gl = canvas.getContext("webgl"); // get a webgl object from it

  try {
    if (gl == null) {
      throw "unable to create gl context -- is your browser gl ready?";
    } else {
      //gl.clearColor(0.0, 0.0, 0.0, 1.0); // use black when we clear the frame buffer
      gl.clearDepth(1.0); // use max when we clear the depth buffer
      gl.enable(gl.DEPTH_TEST); // use hidden surface removal (with zbuffering)
    }
  } // end try

  catch (e) {
    console.log(e);
  } // end catch

} // end setupWebGL

// Helper function to use WEBGL MIPMAP api
// (from https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Tutorial/Using_textures_in_WebGL)

function isPowerOf2(value) {
  return (value & (value - 1)) == 0;
}

// read models in, load them into webgl buffers
function loadModels() {

  // make an ellipsoid, with numLongSteps longitudes.
  // start with a sphere of radius 1 at origin
  // Returns verts, tris and normals.
  function makeEllipsoid(currEllipsoid, numLongSteps) {

    try {
      if (numLongSteps % 2 != 0)
        throw "in makeSphere: uneven number of longitude steps!";
      else if (numLongSteps < 4)
        throw "in makeSphere: number of longitude steps too small!";
      else { // good number longitude steps

        console.log("ellipsoid xyz: " + ellipsoid.x + " " + ellipsoid.y + " " + ellipsoid.z);

        // make vertices
        let ellipsoidUV = [0.5, 0];
        var ellipsoidVertices = [0, -1, 0]; // vertices to return, init to south pole
        var angleIncr = (Math.PI + Math.PI) / numLongSteps; // angular increment
        var latLimitAngle = angleIncr * (Math.floor(numLongSteps / 4) - 1); // start/end lat angle
        var latRadius, latY; // radius and Y at current latitude
        for (var latAngle = -latLimitAngle; latAngle <= latLimitAngle; latAngle += angleIncr) {
          latRadius = Math.cos(latAngle); // radius of current latitude
          latY = Math.sin(latAngle); // height at current latitude
          for (var longAngle = 0; longAngle < 2 * Math.PI; longAngle += angleIncr) // for each long
          {
            const x = latRadius * Math.sin(longAngle);
            const y = latY;
            const z = latRadius * Math.cos(longAngle);
            ellipsoidVertices.push(x, y, z);

            const turn = 0 * Math.PI;
            const u = (modulo_Euclidean(longAngle + turn, 2 * Math.PI)) / (2 * Math.PI);
            const v = (modulo_Euclidean(latAngle + latLimitAngle, 2 * latLimitAngle)) / (latLimitAngle * 2);
            ellipsoidUV.push(u, 1 - v);
          }
        } // end for each latitude
        ellipsoidVertices.push(0, 1, 0); // add north pole
        ellipsoidUV.push(0.5, 1);
        ellipsoidVertices = ellipsoidVertices.map(function(val, idx) { // position and scale ellipsoid
          switch (idx % 3) {
            case 0: // x
              return (val * currEllipsoid.a + currEllipsoid.x);
            case 1: // y
              return (val * currEllipsoid.b + currEllipsoid.y);
            case 2: // z
              return (val * currEllipsoid.c + currEllipsoid.z);
          } // end switch
        });

        // make normals using the ellipsoid gradient equation
        // resulting normals are unnormalized: we rely on shaders to normalize
        var ellipsoidNormals = ellipsoidVertices.slice(); // start with a copy of the transformed verts
        ellipsoidNormals = ellipsoidNormals.map(function(val, idx) { // calculate each normal
          switch (idx % 3) {
            case 0: // x
              return (2 / (currEllipsoid.a * currEllipsoid.a) * (val - currEllipsoid.x));
            case 1: // y
              return (2 / (currEllipsoid.b * currEllipsoid.b) * (val - currEllipsoid.y));
            case 2: // z
              return (2 / (currEllipsoid.c * currEllipsoid.c) * (val - currEllipsoid.z));
          } // end switch
        });

        // make triangles, from south pole to middle latitudes to north pole
        var ellipsoidTriangles = []; // triangles to return
        for (var whichLong = 1; whichLong < numLongSteps; whichLong++) // south pole
          ellipsoidTriangles.push(0, whichLong, whichLong + 1);
        ellipsoidTriangles.push(0, numLongSteps, 1); // longitude wrap tri
        var llVertex; // lower left vertex in the current quad
        for (var whichLat = 0; whichLat < (numLongSteps / 2 - 2); whichLat++) { // middle lats
          for (var whichLong = 0; whichLong < numLongSteps - 1; whichLong++) {
            llVertex = whichLat * numLongSteps + whichLong + 1;
            ellipsoidTriangles.push(llVertex, llVertex + numLongSteps, llVertex + numLongSteps + 1);
            ellipsoidTriangles.push(llVertex, llVertex + numLongSteps + 1, llVertex + 1);
          } // end for each longitude
          ellipsoidTriangles.push(llVertex + 1, llVertex + numLongSteps + 1, llVertex + 2);
          ellipsoidTriangles.push(llVertex + 1, llVertex + 2, llVertex - numLongSteps + 2);
        } // end for each latitude
        for (var whichLong = llVertex + 2; whichLong < llVertex + numLongSteps + 1; whichLong++) // north pole
          ellipsoidTriangles.push(whichLong, ellipsoidVertices.length / 3 - 1, whichLong + 1);
        ellipsoidTriangles.push(ellipsoidVertices.length / 3 - 2, ellipsoidVertices.length / 3 - 1,
          ellipsoidVertices.length / 3 - numLongSteps - 1); // longitude wrap
        return ({ vertices: ellipsoidVertices, normals: ellipsoidNormals, triangles: ellipsoidTriangles, uv: ellipsoidUV });
      } // end if good number longitude steps
    } // end try

    catch (e) {
      console.log(e);
    } // end catch
  } // end make ellipsoid

  try {
    if (inputTriangles == String.null)
      throw "Unable to load triangles file!";
    else {
      var whichSetVert; // index of vertex in current triangle set
      var whichSetTri; // index of triangle in current triangle set
      var vtxToAdd; // vtx coords to add to the coord array
      var normToAdd; // vtx normal to add to the coord array
      var triToAdd; // tri indices to add to the index array
      var maxCorner = vec3.fromValues(Number.MIN_VALUE, Number.MIN_VALUE, Number.MIN_VALUE); // bbox corner
      var minCorner = vec3.fromValues(Number.MAX_VALUE, Number.MAX_VALUE, Number.MAX_VALUE); // other corner

      // process each triangle set to load webgl vertex and triangle buffers
      numTriangleSets = inputTriangles.length; // remember how many tri sets
      function loadTexture(whichSet, whichTexture) {
        const cw = imageCanvas.width;
        const ch = imageCanvas.height;
        // Set up textures
        const texname = whichTexture;

        // WEBGL API CODE FROM
        // https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Tutorial/Using_textures_in_WebGL
        setToName[whichSet] = texname;
        if (texname.slice(-4) === ".png") {
          setToTransparent[whichSet] = true;
        } else {
          setToTransparent[whichSet] = false;
        }
        if (nameToTex[texname] == null) {
          const texture = gl.createTexture();
          gl.bindTexture(gl.TEXTURE_2D, texture);
          const level = 0;
          const internalFormat = gl.RGBA;
          const width = 1;
          const height = 1;
          const border = 0;
          const srcFormat = gl.RGBA;
          const srcType = gl.UNSIGNED_BYTE;
          const pixel = new Uint8Array([0, 0, 255, 255]); // "loading texture" texture
          gl.texImage2D(gl.TEXTURE_2D, level, internalFormat, width, height, border, srcFormat, srcType, pixel);
          // Load texture
          const image = new Image();
          image.crossOrigin = "Anonymous";
          image.src = texname;
          image.onload = function() {
            gl.bindTexture(gl.TEXTURE_2D, texture);
            gl.texImage2D(gl.TEXTURE_2D, level, internalFormat, srcFormat, srcType, image);
            const iw = image.width;
            const ih = image.height;
            if (isPowerOf2(iw) && isPowerOf2(ih)) {
              gl.generateMipmap(gl.TEXTURE_2D);
            } else {
              gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
              gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
              gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
            }
          };
          nameToTex[texname] = texture;
        }
      }
      for (var whichSet = 0; whichSet < numTriangleSets; whichSet++) { // for each tri set
        loadTexture(whichSet, inputTriangles[whichSet].material.texture);
        // set up hilighting, modeling translation and rotation
        inputTriangles[whichSet].center = vec3.fromValues(0, 0, 0);  // center point of tri set
        inputTriangles[whichSet].on = false; // not highlighted
        inputTriangles[whichSet].translation = vec3.fromValues(0, 0, 0); // no translation
        inputTriangles[whichSet].xAxis = vec3.fromValues(1, 0, 0); // model X axis
        inputTriangles[whichSet].yAxis = vec3.fromValues(0, 1, 0); // model Y axis

        // set up the vertex and normal arrays, define model center and axes
        inputTriangles[whichSet].glVertices = []; // flat coord list for webgl
        inputTriangles[whichSet].glNormals = []; // flat normal list for webgl
        var numVerts = inputTriangles[whichSet].vertices.length; // num vertices in tri set
        for (whichSetVert = 0; whichSetVert < numVerts; whichSetVert++) { // verts in set
          vtxToAdd = inputTriangles[whichSet].vertices[whichSetVert]; // get vertex to add
          normToAdd = inputTriangles[whichSet].normals[whichSetVert]; // get normal to add
          inputTriangles[whichSet].glVertices.push(vtxToAdd[0], vtxToAdd[1], vtxToAdd[2]); // put coords in set coord list
          inputTriangles[whichSet].glNormals.push(normToAdd[0], normToAdd[1], normToAdd[2]); // put normal in set coord list
          vec3.max(maxCorner, maxCorner, vtxToAdd); // update world bounding box corner maxima
          vec3.min(minCorner, minCorner, vtxToAdd); // update world bounding box corner minima
          vec3.add(inputTriangles[whichSet].center, inputTriangles[whichSet].center, vtxToAdd); // add to ctr sum
        } // end for vertices in set
        vec3.scale(inputTriangles[whichSet].center, inputTriangles[whichSet].center, 1 / numVerts); // avg ctr sum

        // send the vertex coords and normals to webGL
        vertexBuffers[whichSet] = gl.createBuffer(); // init empty webgl set vertex coord buffer
        gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffers[whichSet]); // activate that buffer
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(inputTriangles[whichSet].glVertices), gl.STATIC_DRAW); // data in
        normalBuffers[whichSet] = gl.createBuffer(); // init empty webgl set normal component buffer
        gl.bindBuffer(gl.ARRAY_BUFFER, normalBuffers[whichSet]); // activate that buffer
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(inputTriangles[whichSet].glNormals), gl.STATIC_DRAW); // data in

        uvBuffers[whichSet] = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, uvBuffers[whichSet]);
        {
          let coords = [];
          for (const coord of inputTriangles[whichSet].uvs) {
            coords.push(1 - coord[0], 1 - coord[1]);
          }
          gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(coords), gl.STATIC_DRAW);
        }

        // set up the triangle index array, adjusting indices across sets
        inputTriangles[whichSet].glTriangles = []; // flat index list for webgl
        triSetSizes[whichSet] = inputTriangles[whichSet].triangles.length; // number of tris in this set
        for (whichSetTri = 0; whichSetTri < triSetSizes[whichSet]; whichSetTri++) {
          triToAdd = inputTriangles[whichSet].triangles[whichSetTri]; // get tri to add
          inputTriangles[whichSet].glTriangles.push(triToAdd[0], triToAdd[1], triToAdd[2]); // put indices in set list
        } // end for triangles in set

        // send the triangle indices to webGL
        triangleBuffers.push(gl.createBuffer()); // init empty triangle index buffer
        gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, triangleBuffers[whichSet]); // activate that buffer
        gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(inputTriangles[whichSet].glTriangles), gl.STATIC_DRAW); // data in

      } // end for each triangle set

    } // end if triangle file loaded
  } // end try

  catch (e) {
    console.log(e);
  } // end catch
} // end load models

// setup the webGL shaders
function setupShaders() {

  // define vertex shader in essl using es6 template strings
  var vShaderCode = `
        attribute vec3 aVertexPosition; // vertex position
        attribute vec3 aVertexNormal; // vertex normal

        uniform mat4 umMatrix; // the model matrix
        uniform mat4 upvmMatrix; // the project view model matrix

        varying vec3 vWorldPos; // interpolated world position of vertex
        varying vec3 vVertexNormal; // interpolated normal for frag shader

        attribute vec2 aTextureCoord;
        varying vec2 vTextureCoord;

        void main(void) {
            vTextureCoord = aTextureCoord;

            // vertex position
            vec4 vWorldPos4 = umMatrix * vec4(aVertexPosition, 1.0);
            vWorldPos = vec3(vWorldPos4.x,vWorldPos4.y,vWorldPos4.z);
            gl_Position = upvmMatrix * vec4(aVertexPosition, 1.0);

            // vertex normal (assume no non-uniform scale)
            vec4 vWorldNormal4 = umMatrix * vec4(aVertexNormal, 0.0);
            vVertexNormal = normalize(vec3(vWorldNormal4.x,vWorldNormal4.y,vWorldNormal4.z));
        }
    `;

  // define fragment shader in essl using es6 template strings
  var fShaderCode = `
        precision mediump float; // set float to medium precision

        // eye location
        uniform vec3 uEyePosition; // the eye's position in world

        // light properties
        uniform vec3 uLightAmbient; // the light's ambient color
        uniform vec3 uLightDiffuse; // the light's diffuse color
        uniform vec3 uLightSpecular; // the light's specular color
        uniform vec3 uLightPosition; // the light's position

        // material properties
        uniform vec3 uAmbient; // the ambient reflectivity
        uniform vec3 uDiffuse; // the diffuse reflectivity
        uniform vec3 uSpecular; // the specular reflectivity
        uniform float uShininess; // the specular exponent
        uniform float uToggleMode;

        uniform sampler2D uSampler;

        // Texture properties
        varying vec2 vTextureCoord;

        // geometry properties
        varying vec3 vWorldPos; // world xyz of fragment
        varying vec3 vVertexNormal; // normal of fragment

        void main(void) {

            // ambient term
            vec3 ambient = uAmbient*uLightAmbient;

            // diffuse term
            vec3 normal = normalize(vVertexNormal);
            vec3 light = normalize(uLightPosition - vWorldPos);
            float lambert = max(0.0,dot(normal,light));
            vec3 diffuse = uDiffuse*uLightDiffuse*lambert; // diffuse term

            // specular term
            vec3 eye = normalize(uEyePosition - vWorldPos);
            vec3 halfVec = normalize(light+eye);
            float highlight = pow(max(0.0,dot(normal,halfVec)),uShininess);
            vec3 specular = uSpecular*uLightSpecular*highlight; // specular term

            // combine to output color
            vec4 colorOut = vec4(ambient + diffuse + specular, 1.0); // no specular yet
            vec4 texColor = texture2D(uSampler, vec2(vTextureCoord[0], vTextureCoord[1]));
            if (uToggleMode == 1.0) {
                gl_FragColor = texColor * colorOut;
            } else if (uToggleMode == 2.0) {
                gl_FragColor = colorOut;
            } else {
                gl_FragColor = texColor;
            }

            // float uvSum = vTextureCoord[0] + vTextureCoord[1];
            // uvSum /= 2.0;
            // gl_FragColor = vec4(uvSum, uvSum, uvSum, 1);
        }
    `;

  try {
    var fShader = gl.createShader(gl.FRAGMENT_SHADER); // create frag shader
    gl.shaderSource(fShader, fShaderCode); // attach code to shader
    gl.compileShader(fShader); // compile the code for gpu execution

    var vShader = gl.createShader(gl.VERTEX_SHADER); // create vertex shader
    gl.shaderSource(vShader, vShaderCode); // attach code to shader
    gl.compileShader(vShader); // compile the code for gpu execution

    if (!gl.getShaderParameter(fShader, gl.COMPILE_STATUS)) { // bad frag shader compile
      gl.deleteShader(fShader);
      throw "error during fragment shader compile: " + gl.getShaderInfoLog(fShader);
    } else if (!gl.getShaderParameter(vShader, gl.COMPILE_STATUS)) { // bad vertex shader compile
      gl.deleteShader(vShader);
      throw "error during vertex shader compile: " + gl.getShaderInfoLog(vShader);
    } else { // no compile errors
      var shaderProgram = gl.createProgram(); // create the single shader program
      gl.attachShader(shaderProgram, fShader); // put frag shader in program
      gl.attachShader(shaderProgram, vShader); // put vertex shader in program
      gl.linkProgram(shaderProgram); // link program into gl context

      if (!gl.getProgramParameter(shaderProgram, gl.LINK_STATUS)) { // bad program link
        throw "error during shader program linking: " + gl.getProgramInfoLog(shaderProgram);
      } else { // no shader program link errors
        gl.useProgram(shaderProgram); // activate shader program (frag and vert)

        uvAttribLoc = gl.getAttribLocation(shaderProgram, "aTextureCoord");
        gl.enableVertexAttribArray(uvAttribLoc); // connect attrib to array

        // locate and enable vertex attributes
        vPosAttribLoc = gl.getAttribLocation(shaderProgram, "aVertexPosition"); // ptr to vertex pos attrib
        gl.enableVertexAttribArray(vPosAttribLoc); // connect attrib to array
        vNormAttribLoc = gl.getAttribLocation(shaderProgram, "aVertexNormal"); // ptr to vertex normal attrib
        gl.enableVertexAttribArray(vNormAttribLoc); // connect attrib to array

        // locate vertex uniforms
        mMatrixULoc = gl.getUniformLocation(shaderProgram, "umMatrix"); // ptr to mmat
        pvmMatrixULoc = gl.getUniformLocation(shaderProgram, "upvmMatrix"); // ptr to pvmmat

        // locate fragment uniforms
        var eyePositionULoc = gl.getUniformLocation(shaderProgram, "uEyePosition"); // ptr to eye position
        var lightAmbientULoc = gl.getUniformLocation(shaderProgram, "uLightAmbient"); // ptr to light ambient
        var lightDiffuseULoc = gl.getUniformLocation(shaderProgram, "uLightDiffuse"); // ptr to light diffuse
        var lightSpecularULoc = gl.getUniformLocation(shaderProgram, "uLightSpecular"); // ptr to light specular
        var lightPositionULoc = gl.getUniformLocation(shaderProgram, "uLightPosition"); // ptr to light position
        ambientULoc = gl.getUniformLocation(shaderProgram, "uAmbient"); // ptr to ambient
        diffuseULoc = gl.getUniformLocation(shaderProgram, "uDiffuse"); // ptr to diffuse
        specularULoc = gl.getUniformLocation(shaderProgram, "uSpecular"); // ptr to specular
        shininessULoc = gl.getUniformLocation(shaderProgram, "uShininess"); // ptr to shininess
        samplerULoc = gl.getUniformLocation(shaderProgram, "uSampler"); // ptr to tex
        modeULoc = gl.getUniformLocation(shaderProgram, "uToggleMode"); // ptr to toggle mode var
        gl.uniform1f(modeULoc, blendMode); // pass in the mode value

        // pass global constants into fragment uniforms
        gl.uniform3fv(eyePositionULoc, Eye); // pass in the eye's position
        gl.uniform3fv(lightAmbientULoc, lightAmbient); // pass in the light's ambient emission
        gl.uniform3fv(lightDiffuseULoc, lightDiffuse); // pass in the light's diffuse emission
        gl.uniform3fv(lightSpecularULoc, lightSpecular); // pass in the light's specular emission
        gl.uniform3fv(lightPositionULoc, lightPosition); // pass in the light's position
      } // end if no shader program link errors
    } // end if no compile errors
  } // end try

  catch (e) {
    console.log(e);
  } // end catch
} // end setup shaders

let fireOnce = true;

// Load input game data

let moveTetrominosLeft = [];
let moveTetrominosRight = [];
let rotateTetrominosCC = [];
let hideTetrominoCubes = [];

function spawnTetromino() {
  let which = Math.floor(Math.random() * makeTetrominos.length);
  let where = Math.floor(Math.random() * (rangeTetrominos[which] + 1));
  let pos = [];
  for (let ppos of tetrominoPositions[which][0]) {
    pos.push([ppos[0] + where, ppos[1]]);
  }
  let tetromino =
  {
    "kind": which,
    "rotation": 0,
    "depth": 0,
    "disp": 0,
    "translation": vec3.fromValues(-1 * where * 0.030, 0, 0),
    "active": true,
  };

  tetrominoList.push(tetromino);

  function clickdown() {
    if (tetromino.active) {
      vec3.subtract(tetromino.translation, tetromino.translation, vec3.fromValues(0, 0.03, 0));
      tetromino.depth += 1;
    }
  }
  gravityHook.push(() => clickdown());
  gravityCollisionHook.push(() => {
    if (tetromino.active) {
      let model = tetrominoCubes[tetromino.kind][tetromino.rotation];
      for (let i = 0; i < model.length; i++) {
        for (let j = 0; j < model[i].length; j++) {
          for (let k = model[i][j][0]; k <= model[i][j][1]; k++) {
            if (k > 0) {
              let cube = {};
              let currSet = inputTriangles[k];
              cube.set = currSet;
              cube.setK = k;
              cube.translation = vec3.create();
              vec3.set(cube.translation, 0, 0, 0);
              vec3.add(cube.translation, cube.translation, currSet.cubeTranslation);
              vec3.add(cube.translation, cube.translation, tetromino.translation);
              cube.depth = tetromino.depth + currSet.cubeDepth;
              cube.disp = tetromino.disp + currSet.cubeDisp;
              cube.active = true;
              allCubes.push(cube);
              // tetromino.cubes.push(cube);
              lineClearHook.push((depth) => {
                if (cube.active) {
                  if (depth < cube.depth) {
                    return;
                  }
                  if (depth == cube.depth) {
                    cube.active = false;
                    vec3.subtract(cube.translation, cube.translation, vec3.fromValues(0, 0.0, 100));
                  }
                  if (depth > cube.depth) {
                    vec3.subtract(cube.translation, cube.translation, vec3.fromValues(0, 0.03, 0));
                  }
                }
              });
            }
          }
        }
      }
    }

    tetromino.active = false;
  });

  rotateTetrominosCC.push(() => {
    if (tetromino.active) {
      if (true) { // Change to rot. colsn. detect
        let setTo = tetromino.rotation;
        setTo += 1;
        if (setTo >= tetrominoIndices[which].length) {
          setTo = 0;
        }

        // console.log("new posns: ", tetrominoPositions[which][setTo]);
        for (const pair of tetrominoPositions[which][setTo]) {
          // console.log("i tested: ", gameWhere + pair[0]);
          if (collisionDetect(23 - (pair[1] - gameHeight), gameWhere + pair[0])) {
            // console.log("colsn detected");
            return;
          }
        }

        // console.log("setTo is: ", setTo);

        for (let i = 0; i < gameBoard.length; i++) {
          for (let j = 0; j < gameBoard[i].length; j++) {
            if (gameBoard[i][j] == 1) {
              gameBoard[i][j] = 0;
            }
          }
        }

        // console.log("=====================================");
        for (const pair of tetrominoPositions[which][setTo]) {
          // console.log("GameHeight is: ", gameHeight);
          // console.log("Inserting point", pair);
          // console.log("calculated as row", pair[1] - gameHeight);
          // console.log("col", gameWhere + pair[0]);
          gameBoard[23 - (pair[1] - gameHeight)][gameWhere + pair[0]] = 1;
        }
        // console.log("=====================================");

        tetromino.rotation = setTo;
      }
    }
  });

  moveTetrominosLeft.push(() => {
    if (tetromino.active) {
      if (movePieceLeft()) {
        vec3.add(tetromino.translation, tetromino.translation, vec3.fromValues(0.030, 0, 0));
        tetromino.disp += 1;
      }
    }
  });

  moveTetrominosRight.push(() => {
    if (tetromino.active) {
      if (movePieceRight()) {
        vec3.subtract(tetromino.translation, tetromino.translation, vec3.fromValues(0.030, 0, 0));
        tetromino.disp -= 1;
      }
    }
  });

  // Enter data into gameboard
  gameHeight = 0;
  gameWhere = where;
  for (const pair of pos) {
    gameBoard[23 - pair[1]][pair[0]] = 1;
  }
}

spawnTetromino();

// const GAME_LOOP_TIME = 1000;

// function gameloop() {
//   setTimeout(() => {
//     if (gravity()) {
//       spawnTetromino();
//     }
//     if (gameIsOver()) {
//       console.log("game over");
//     } else {
//       gameloop();
//     }
//   }, GAME_LOOP_TIME);
// }

let everyOther = true;

// render the loaded model
function renderModels() {

  // construct the model transform matrix, based on model state
  function makeModelTransform(currModel) {
    var zAxis = vec3.create();
    var sumRotation = mat4.create();
    var temp = mat4.create();
    var negCtr = vec3.create();

    // move the model to the origin
    mat4.fromTranslation(mMatrix, vec3.negate(negCtr, currModel.center));

    // scale for highlighting if needed
    if (currModel.on)
      mat4.multiply(mMatrix, mat4.fromScaling(temp, vec3.fromValues(1.2, 1.2, 1.2)), mMatrix); // S(1.2) * T(-ctr)

    // rotate the model to current interactive orientation
    vec3.normalize(zAxis, vec3.cross(zAxis, currModel.xAxis, currModel.yAxis)); // get the new model z axis
    mat4.set(sumRotation, // get the composite rotation
      currModel.xAxis[0], currModel.yAxis[0], zAxis[0], 0,
      currModel.xAxis[1], currModel.yAxis[1], zAxis[1], 0,
      currModel.xAxis[2], currModel.yAxis[2], zAxis[2], 0,
      0, 0, 0, 1);
    mat4.multiply(mMatrix, sumRotation, mMatrix); // R(ax) * S(1.2) * T(-ctr)

    // translate back to model center
    mat4.multiply(mMatrix, mat4.fromTranslation(temp, currModel.center), mMatrix); // T(ctr) * R(ax) * S(1.2) * T(-ctr)

    // translate model to current interactive orientation
    mat4.multiply(mMatrix, mat4.fromTranslation(temp, currModel.translation), mMatrix); // T(pos)*T(ctr)*R(ax)*S(1.2)*T(-ctr)

  } // end make model transform

  // var hMatrix = mat4.create(); // handedness matrix
  var pMatrix = mat4.create(); // projection matrix
  var vMatrix = mat4.create(); // view matrix
  var mMatrix = mat4.create(); // model matrix
  var pvMatrix = mat4.create(); // hand * proj * view matrices
  var pvmMatrix = mat4.create(); // hand * proj * view * model matrices

  // TODO remove timeout
  // set up frame render callback
  setTimeout(() => window.requestAnimationFrame(renderModels), (1000 / 25));

  if (everyOther) {
    if (!gameIsOver()) {
      if (tetMovedRight) {
        for (const fn of moveTetrominosRight) {
          fn();
        }
      }
      tetMovedRight = false;
      if (tetMovedLeft) {
        for (const fn of moveTetrominosLeft) {
          fn();
        }
      }
      tetMovedLeft = false;
      if (rotatedCC) {
        for (const fn of rotateTetrominosCC) {
          fn();
        }
      }
      rotatedCC = false;
      if (tetMovedDown) {
        for (let i = 0; i < 3; i++) {
          gravity();
        }
      }
      if (spacePressed) {
        for (let i = 0; i < 25; i++) {
          gravity();
        }
      }
      spacePressed = false;
      tetMovedDown = false;
      if (noActivePiece() && !gameIsOver()) {
        spawnTetromino();
      } else {
        gravity();
        clearLines();
      }
    }
  }
  everyOther = !everyOther;

  gl.depthMask(true);
  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT); // clear frame/depth buffers
  gl.enable(gl.BLEND);
  // From http://learnwebgl.brown37.net/11_advanced_rendering/alpha_blending.html
  gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
  gl.enable(gl.SAMPLE_ALPHA_TO_COVERAGE);
  // gl.disable(gl.CULL_FACE);

  // set up projection and view
  // mat4.fromScaling(hMatrix,vec3.fromValues(-1,1,1)); // create handedness matrix
  mat4.perspective(pMatrix, 0.5 * Math.PI, 1, 0.1, 10); // create projection matrix
  mat4.lookAt(vMatrix, Eye, Center, Up); // create view matrix
  mat4.multiply(pvMatrix, pvMatrix, pMatrix); // projection
  mat4.multiply(pvMatrix, pvMatrix, vMatrix); // projection * view

  // render each triangle set
  function renderATriSet(whichTriSet, currSet) {

    // make model transform, add to view project
    makeModelTransform(currSet);
    mat4.multiply(pvmMatrix, pvMatrix, mMatrix); // project * view * model
    gl.uniformMatrix4fv(mMatrixULoc, false, mMatrix); // pass in the m matrix
    gl.uniformMatrix4fv(pvmMatrixULoc, false, pvmMatrix); // pass in the hpvm matrix

    // reflectivity: feed to the fragment shader
    gl.uniform3fv(ambientULoc, currSet.material.ambient); // pass in the ambient reflectivity
    gl.uniform3fv(diffuseULoc, currSet.material.diffuse); // pass in the diffuse reflectivity
    gl.uniform3fv(specularULoc, currSet.material.specular); // pass in the specular reflectivity
    gl.uniform1f(shininessULoc, currSet.material.n); // pass in the specular exponent

    // vertex buffer: activate and feed into vertex shader
    gl.bindBuffer(gl.ARRAY_BUFFER, uvBuffers[whichTriSet]);
    gl.vertexAttribPointer(uvAttribLoc, 2, gl.FLOAT, false, 0, 0);
    gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffers[whichTriSet]); // activate
    gl.vertexAttribPointer(vPosAttribLoc, 3, gl.FLOAT, false, 0, 0); // feed
    gl.bindBuffer(gl.ARRAY_BUFFER, normalBuffers[whichTriSet]); // activate
    gl.vertexAttribPointer(vNormAttribLoc, 3, gl.FLOAT, false, 0, 0); // feed

    // Texture these little suckers
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, nameToTex[setToName[whichTriSet]]);
    gl.uniform1i(samplerULoc, 0);

    // triangle buffer: activate and render
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, triangleBuffers[whichTriSet]); // activate
    gl.drawElements(gl.TRIANGLES, 3 * triSetSizes[whichTriSet], gl.UNSIGNED_SHORT, 0); // render
  }

  // render each ellipsoid
  function renderAEllipsoid(ellipsoid, whichEllipsoid) {

    // define model transform, premult with pvmMatrix, feed to vertex shader
    makeModelTransform(ellipsoid);
    pvmMatrix = mat4.multiply(pvmMatrix, pvMatrix, mMatrix); // premultiply with pv matrix
    gl.uniformMatrix4fv(mMatrixULoc, false, mMatrix); // pass in model matrix
    gl.uniformMatrix4fv(pvmMatrixULoc, false, pvmMatrix); // pass in project view model matrix

    // reflectivity: feed to the fragment shader
    gl.uniform3fv(ambientULoc, ellipsoid.ambient); // pass in the ambient reflectivity
    gl.uniform3fv(diffuseULoc, ellipsoid.diffuse); // pass in the diffuse reflectivity
    gl.uniform3fv(specularULoc, ellipsoid.specular); // pass in the specular reflectivity
    gl.uniform1f(shininessULoc, ellipsoid.n); // pass in the specular exponent

    gl.bindBuffer(gl.ARRAY_BUFFER, uvBuffers[whichEllipsoid + numTriangleSets]);
    gl.vertexAttribPointer(uvAttribLoc, 2, gl.FLOAT, false, 0, 0);
    gl.bindBuffer(gl.ARRAY_BUFFER, vertexBuffers[numTriangleSets + whichEllipsoid]); // activate vertex buffer
    gl.vertexAttribPointer(vPosAttribLoc, 3, gl.FLOAT, false, 0, 0); // feed vertex buffer to shader
    gl.bindBuffer(gl.ARRAY_BUFFER, normalBuffers[numTriangleSets + whichEllipsoid]); // activate normal buffer
    gl.vertexAttribPointer(vNormAttribLoc, 3, gl.FLOAT, false, 0, 0); // feed normal buffer to shader
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, triangleBuffers[numTriangleSets + whichEllipsoid]); // activate tri buffer

    // Texture these round babies
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, nameToTex[setToName[numTriangleSets + whichEllipsoid]]);
    gl.uniform1i(samplerULoc, 0);

    // draw a transformed instance of the ellipsoid
    gl.drawElements(gl.TRIANGLES, triSetSizes[numTriangleSets + whichEllipsoid], gl.UNSIGNED_SHORT, 0); // render
  }

  // Render transparent:
  gl.depthMask(false || blendMode == 2.0);
  {
    for (let whichTriSet = 0; whichTriSet < numTriangleSets; whichTriSet++) {
      let currSet = inputTriangles[whichTriSet];
      if (setToTransparent[whichTriSet]) {
        renderATriSet(whichTriSet, currSet);
      }
    }

  }
  gl.depthMask(true);
  // Render Opaque:
  {
    for (let whichTriSet = 0; whichTriSet < numTriangleSets; whichTriSet++) {
      let currSet = inputTriangles[whichTriSet];
      if (!setToTransparent[whichTriSet] && whichTriSet > tetrominoEnd || whichTriSet < tetrominoStart) {
        renderATriSet(whichTriSet, currSet);
      }
    }
  }

  // Render Tetrominos:
  const totalTrans = vec3.create();
  {
    for (const tetromino of tetrominoList) {
      if (tetromino.active) {
        let model = tetrominoCubes[tetromino.kind][tetromino.rotation];
        for (let i = 0; i < model.length; i++) {
          for (let j = 0; j < model[i].length; j++) {
            for (let k = model[i][j][0]; k <= model[i][j][1]; k++) {
              if (k > 0) {
                let currSet = inputTriangles[k];
                vec3.set(totalTrans, 0, 0, 0);
                vec3.add(totalTrans, totalTrans, currSet.cubeTranslation);
                vec3.add(totalTrans, totalTrans, tetromino.translation);
                currSet.translation = totalTrans;
                renderATriSet(k, currSet);
              }
            }
          }
        }
      } else {
        for (const cube of allCubes) {
          if (cube.active) {
            let k = cube.setK;
            let currSet = cube.set;
            currSet.translation = cube.translation;
            renderATriSet(k, currSet);
          }
        }
      }
    }
  }
} // end render model

/* MAIN -- HERE is where execution begins after window load */

function main() {

  setupWebGL(); // set up the webGL environment
  loadModels(); // load in the models from tri file
  setupShaders(); // setup the webGL shaders
  renderModels(); // draw the triangles using webGL

} // end main
