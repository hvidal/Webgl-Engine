
const
	glMat4 = require('gl-mat4'),
	stanfordDragon = require('stanford-dragon/4');

class Texture {
	constructor (gl, slot, internalFormat, width, height, format, pixels) {
		this.gl = gl;
		this.slot = slot;
		this.internalFormat = internalFormat;
		this.width = width;
		this.height = height;
		this.format = format;
		this.type = gl.UNSIGNED_BYTE;
		this.pixels = pixels;
		this.texture = gl.createTexture();
	}

	bind() {
		this.gl.activeTexture(this.gl.TEXTURE0 + this.slot);
		this.gl.bindTexture(this.gl.TEXTURE_2D, this.texture);
		return this;
	}

	linear() {
		this.bind();
		this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_MAG_FILTER, this.gl.LINEAR);
		this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_MIN_FILTER, this.gl.LINEAR);
		return this;
	}

	nearest() {
		this.bind();
		this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_MAG_FILTER, this.gl.NEAREST);
		this.gl.texParameteri(this.gl.TEXTURE_2D, this.gl.TEXTURE_MIN_FILTER, this.gl.NEAREST);
		return this;
	}

	image() {
		this.gl.texImage2D(this.gl.TEXTURE_2D, 0, this.internalFormat, this.width, this.height, 0, this.format, this.type, this.pixels);
		return this;
	}

	unbind() {
		this.gl.bindTexture(this.gl.TEXTURE_2D, null);
		return this;
	}

	getSlot() {
		return this.slot;
	}
}

class Framebuffer {
	constructor (gl) {
		this.gl = gl;
		this.buffer = gl.createFramebuffer();
	}

	bind() {
		this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, this.buffer);
	}

	unbind() {
		this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, null);
	}
}

class Renderbuffer {
	constructor (gl) {
		this.gl = gl;
		this.buffer = gl.createRenderbuffer();
	}

	bind() {
		this.gl.bindRenderbuffer(this.gl.RENDERBUFFER, this.buffer);
		return this;
	}

	storage(size) {
		this.gl.renderbufferStorage(this.gl.RENDERBUFFER, this.gl.DEPTH_COMPONENT16, size, size);
		return this;
	}

	unbind() {
		this.gl.bindRenderbuffer(this.gl.RENDERBUFFER, null);
		this;
	}
}

class Shader {
	/**
	* @type {WebGLRenderingContext} gl
	* @type {String} vertexGLSL vertex shader source
	* @type {String} fragmentGLSL fragment shader source
	*/
	constructor (gl, vertexGLSL, fragmentGLSL) {
		this.gl = gl;
		this.uniformCache = {};
		const vertexShader = gl.createShader(gl.VERTEX_SHADER);
		gl.shaderSource(vertexShader, vertexGLSL);
		gl.compileShader(vertexShader);

		const fragmentShader = gl.createShader(gl.FRAGMENT_SHADER)
		gl.shaderSource(fragmentShader, fragmentGLSL);
		gl.compileShader(fragmentShader);

		this.shaderProgram = gl.createProgram();
		gl.attachShader(this.shaderProgram, vertexShader);
		gl.attachShader(this.shaderProgram, fragmentShader);
		gl.linkProgram(this.shaderProgram);
	}

	getAttributeLocation(name) {
		const location = this.gl.getAttribLocation(this.shaderProgram, name);
		this.gl.enableVertexAttribArray(location);
		return location;
	}

	use() {
		this.gl.useProgram(this.shaderProgram);
	}

	getUniformLocation(name) {
		if (!this.uniformCache[name]) {
			this.uniformCache[name] = this.gl.getUniformLocation(this.shaderProgram, name);
		}
		return this.uniformCache[name];
	}

	setTexture(uniformName, texture) {
		texture.bind();
		this.gl.uniform1i(this.getUniformLocation(uniformName), texture.getSlot());
	}

	setMatrix4fv(uniformName, matrix) {
		this.gl.uniformMatrix4fv(this.getUniformLocation(uniformName), false, matrix);
	}

	set3fv(uniformName, value) {
		this.gl.uniform3fv(this.getUniformLocation(uniformName), value);
	}
}

/**
 * This is a camera that always looks at a fixed point. The viewer can rotate the camera by dragging the mouse or
 * swiping the screen.
 */
class RotationCamera {

	constructor (centerPoint, xRotation, yRotation) {
		this.centerPoint = centerPoint;
		this.upVector = [0, 1, 0];
		this.xRotation = xRotation;
		this.yRotation = yRotation;
	}

	registerEvents(canvas) {
		let downPoint = null;

		const onDown = (e) => {
			downPoint = {
				x: e.pageX || e.touches[0].clientX,
				y: e.pageY || e.touches[0].clientY
			};
		};

		const onRelease = () => {
			downPoint = null;
		};

		const onMove = (e) => {
			e.preventDefault();
			if (downPoint) {
				this.xRotation += (e.pageY - downPoint.y) / 50;
				this.yRotation -= (e.pageX - downPoint.x) / 50;

				this.xRotation = Math.min(this.xRotation, Math.PI / 2.5);
				this.xRotation = Math.max(this.xRotation, 0.1);

				downPoint.x = e.pageX;
				downPoint.y = e.pageY;
			}
		};

		canvas.addEventListener('mousedown', onDown);
		canvas.addEventListener('touchstart', onDown);

		canvas.addEventListener('mouseup', onRelease);
		canvas.addEventListener('mouseout', onRelease);

		canvas.addEventListener('mousemove', onMove);
		canvas.addEventListener('touchmove', onMove);
	}

	update() {
		let m = glMat4.create();
		glMat4.translate(m, m, [0, 0, 45]);
		const xRotMatrix = glMat4.create();
		const yRotMatrix = glMat4.create();
		glMat4.rotateX(xRotMatrix, xRotMatrix, -this.xRotation);
		glMat4.rotateY(yRotMatrix, yRotMatrix, this.yRotation);
		glMat4.multiply(m, xRotMatrix, m);
		glMat4.multiply(m, yRotMatrix, m);
		return glMat4.lookAt(m, [m[12], m[13], m[14]], this.centerPoint, this.upVector);
	}
}

class Scene {

	constructor (properties) {
		this.clearColor = properties['clearColor'] || [1, 1, 1, 1];
		this.sceneObjects = {
			castShadow: [],
			receiveShadow: []
		};
		this.initCanvas();
		this.initGLContext();
	}

	/** @private */
	initCanvas() {
		this.canvas = document.createElement('canvas');
		this.canvas.style.display = 'block';
		document.body.appendChild(this.canvas);
		document.body.style.margin = 0;

		const resize = () => {
			this.canvas.width = window.innerWidth;
			this.canvas.height = window.innerHeight;
		};
		resize();
		window.addEventListener('resize', resize);
	}

	/** @private */
	initGLContext() {
		this.gl = this.canvas.getContext('webgl');
		this.gl.enable(this.gl.DEPTH_TEST);
    }

	/**
	 * @type {SceneObject} sceneObject
	 * @type {Boolean} castShadow
	 * @type {Boolean} receiveShadow
	 */
	addSceneObject(sceneObject, castShadow, receiveShadow) {
		castShadow && this.sceneObjects.castShadow.push(sceneObject);
		receiveShadow && this.sceneObjects.receiveShadow.push(sceneObject);
	}

	render() {
		gl.viewport(0, 0, this.canvas.width, this.canvas.height);
		gl.clearColor(this.clearColor[0], this.clearColor[1], this.clearColor[2], this.clearColor[3]);
		gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
	}
}

class ShadowMap {
	/**
	 * @type {WebGLRenderingContext} gl
	 * @type {Number} textureSize
	*/
	constructor (gl, textureSize) {
		this.gl = gl;
		this.textureSize = textureSize;
		this._isRendering = false;

		const vertexGLSL = `
			attribute vec3 position;

			uniform mat4 P;
			uniform mat4 MV;

			void main (void) {
				gl_Position = P * MV * vec4(position, 1.0);
			}
		`;

		const fragmentGLSL = `
			precision mediump float;

			const vec4 bitShift = vec4(
				256 * 256 * 256,
				256 * 256,
				256,
				1.0
			);
			const vec4 bitMask = vec4(
				0,
				1.0 / 256.0,
				1.0 / 256.0,
				1.0 / 256.0
			);

			vec4 encodeFloat (float depth) {
				vec4 comp = fract(depth * bitShift);
				comp -= comp.xxyz * bitMask;
				return comp;
			}

			void main (void) {
				gl_FragColor = encodeFloat(gl_FragCoord.z); // encode the distance
			}
		`;
		this.shader = new Shader(gl, vertexGLSL, fragmentGLSL);

		/**
		 * Light shader setup
		 */

		this.shader.use();
		this.shadowFramebuffer = new Framebuffer(gl);
		this.shadowFramebuffer.bind();

		this.shadowDepthTexture =
			new Texture(gl, 0, gl.RGBA, shadowTextureSize, shadowTextureSize, gl.RGBA, null)
				.nearest()
				.image();

		const shadowRenderbuffer =
			new Renderbuffer(gl, this.textureSize)
				.bind()
				.storage(this.textureSize);

		gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, this.shadowDepthTexture.texture, 0);
		gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, shadowRenderbuffer.buffer);

		this.shadowDepthTexture.unbind();
		shadowRenderbuffer.unbind();

		this.lightProjectionMatrix = glMat4.ortho([], -40, 40, -40, 40, -40.0, 80);
		this.lightViewMatrix = glMat4.lookAt([], [0, 2, -3], [0, 0, 0], [0, 1, 0]);

		var shadowPMatrix = gl.getUniformLocation(this.shader.shaderProgram, 'P');
		var shadowMVMatrix = gl.getUniformLocation(this.shader.shaderProgram, 'MV');

		gl.uniformMatrix4fv(shadowPMatrix, false, this.lightProjectionMatrix);
		gl.uniformMatrix4fv(shadowMVMatrix, false, this.lightViewMatrix);

		this.shadowFramebuffer.unbind();
	}

	setVars(shader) {
		shader.setTexture('depthColorTexture', this.shadowDepthTexture);
		shader.setMatrix4fv('lightMViewMatrix', this.lightViewMatrix);
		shader.setMatrix4fv('lightProjectionMatrix', this.lightProjectionMatrix);
		shader.setMatrix4fv('P', glMat4.perspective([], Math.PI / 3, 1, 0.01, 900));
	}

	start() {
		this.shader.use();
		this.shadowFramebuffer.bind();
		this.gl.viewport(0, 0, this.textureSize, this.textureSize);
		this.gl.clearColor(0, 0, 0, 1);
		this.gl.clearDepth(1.0);
		this.gl.clear(this.gl.COLOR_BUFFER_BIT | this.gl.DEPTH_BUFFER_BIT);
		this._isRendering = true;
	}

	end() {
		this._isRendering = false;
		this.gl.bindFramebuffer(this.gl.FRAMEBUFFER, null);
	}

	isRendering() {
		return this._isRendering;
	}
}

var shadowTextureSize = 1024;

const cameraVertexGLSL = `
	attribute vec3 position;

	uniform mat4 P;
	uniform mat4 MV;
	uniform mat4 lightMViewMatrix;
	uniform mat4 lightProjectionMatrix;

	// Used to normalize our coordinates from clip space to (0 - 1)
	// so that we can access the corresponding point in our depth color texture
	const mat4 texUnitConverter = mat4(
		0.5, 0.0, 0.0, 0.0,
		0.0, 0.5, 0.0, 0.0,
		0.0, 0.0, 0.5, 0.0,
		0.5, 0.5, 0.5, 1.0);

	varying vec2 vDepthUv;
	varying vec4 shadowPos;

	void main (void) {
	  gl_Position = P * MV * vec4(position, 1.0);
	  shadowPos = texUnitConverter * lightProjectionMatrix * lightMViewMatrix * vec4(position, 1.0);
	}
`;

const cameraFragmentGLSL = `
	precision mediump float;

	varying vec2 vDepthUv;
	varying vec4 shadowPos;

	uniform sampler2D depthColorTexture;
	uniform vec3 uColor;

	float decodeFloat (vec4 color) {
	  const vec4 bitShift = vec4(
			1.0 / (256.0 * 256.0 * 256.0),
			1.0 / (256.0 * 256.0),
			1.0 / 256.0,
			1
	  );
	  return dot(color, bitShift);
	}

	void main(void) {
	  vec3 fragmentDepth = shadowPos.xyz;
	  float shadowAcneRemover = 0.007;
	  fragmentDepth.z -= shadowAcneRemover;

	  float texelSize = 1.0 / ${shadowTextureSize}.0;
	  float amountInLight = 0.0;

	  // Check whether or not the current fragment and the 8 fragments surrounding
	  // the current fragment are in the shadow. We then average out whether or not
	  // all of these fragments are in the shadow to determine the shadow contribution
	  // of the current fragment.
	  // So if 4 out of 9 fragments that we check are in the shadow then we'll say that
	  // this fragment is 4/9ths in the shadow so it'll be a little brighter than something
	  // that is 9/9ths in the shadow.
	  for (int x = -1; x <= 1; x++) {
			for (int y = -1; y <= 1; y++) {
				float texelDepth = decodeFloat(texture2D(depthColorTexture, fragmentDepth.xy + vec2(x, y) * texelSize));
				if (fragmentDepth.z < texelDepth) {
					amountInLight += 1.0;
				}
			}
	  }
	  amountInLight /= 9.0;

	  gl_FragColor = vec4(amountInLight * uColor, 1.0);
	}
`;

class SceneObject {}

class Floor extends SceneObject {

	constructor (gl, vertexPositionAttrib) {
		super();
		this.gl = gl;
		this.vertexPositionAttrib = vertexPositionAttrib;

		const vertices = [
		  -30.0, 0.0, 30.0, // Bottom Left (0)
		  30.0, 0.0, 30.0, // Bottom Right (1)
		  30.0, 0.0, -30.0, // Top Right (2)
		  -30.0, 0.0, -30.0 // Top Left (3)
		];

		const indices = [0, 1, 2, 0, 2, 3];
		this.indexCount = indices.length;

		this.vbo = gl.createBuffer();
		gl.bindBuffer(gl.ARRAY_BUFFER, this.vbo);
		gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
		gl.vertexAttribPointer(vertexPositionAttrib, 3, gl.FLOAT, false, 0, 0);

		this.ibo = gl.createBuffer();
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.ibo);
		gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(indices), gl.STATIC_DRAW);
	}

	bind() {
		this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.vbo);
		this.gl.bindBuffer(this.gl.ELEMENT_ARRAY_BUFFER, this.ibo);
		this.gl.vertexAttribPointer(this.vertexPositionAttrib, 3, this.gl.FLOAT, false, 0, 0);
	}

	render() {
		this.gl.drawElements(this.gl.TRIANGLES, this.indexCount, this.gl.UNSIGNED_SHORT, 0);
	}
}

class Dragon extends SceneObject {

	constructor (gl, vertexPositionAttrib) {
		super();
		this.gl = gl;
		this.vertexPositionAttrib = vertexPositionAttrib;

		this.dragonRotateY = 0;

		// standford dragon comes with nested arrays that look like this
		// [[0, 0, 0,], [1, 0, 1]]
		// We flatten them to this so that we can buffer them onto the GPU
		// [0, 0, 0, 1, 0, 1]
		const dragonPositions = stanfordDragon.positions.reduce(function (all, vertex) {
			// Scale everything down by 10
			all.push(vertex[0] / 10);
			all.push(vertex[1] / 10);
			all.push(vertex[2] / 10);
			return all;
		}, []);

		const dragonIndices = stanfordDragon.cells.reduce(function (all, vertex) {
			all.push(vertex[0]);
			all.push(vertex[1]);
			all.push(vertex[2]);
			return all;
		}, []);

		this.indexCount = dragonIndices.length;

		this.vbo = gl.createBuffer();
		gl.bindBuffer(gl.ARRAY_BUFFER, this.vbo);
		gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(dragonPositions), gl.STATIC_DRAW);
		gl.vertexAttribPointer(vertexPositionAttrib, 3, gl.FLOAT, false, 0, 0);

		this.ibo = gl.createBuffer();
		gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.ibo);
		gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Uint16Array(dragonIndices), gl.STATIC_DRAW);
	}

	bind() {
		this.gl.bindBuffer(this.gl.ARRAY_BUFFER, this.vbo);
		this.gl.bindBuffer(this.gl.ELEMENT_ARRAY_BUFFER, this.ibo);
		this.gl.vertexAttribPointer(this.vertexPositionAttrib, 3, this.gl.FLOAT, false, 0, 0);
	}

	animate() {
		this.dragonRotateY += 0.01;
	}

	/**
	* @param {ShadowMap} shadowMap
	*/
	render(shadowMap, camera, cameraShader) {
		const m = dragon.getModelMatrix();

		if (shadowMap.isRendering()) {
			glMat4.multiply(m, shadowMap.lightViewMatrix, m);
			shadowMap.shader.setMatrix4fv('MV', m);
		} else {
			// We use the light's model view matrix of our dragon so that our camera knows if parts of the dragon are in the shadow
			const lightMVMatrix = glMat4.create();
			glMat4.multiply(lightMVMatrix, shadowMap.lightViewMatrix, m);
			cameraShader.setMatrix4fv('lightMViewMatrix', lightMVMatrix);
			cameraShader.setMatrix4fv('MV', glMat4.multiply(m, camera, m));
			cameraShader.set3fv('uColor', [0.36, 0.66, 0.8]);
			shadowMap.shadowDepthTexture.bind();
		}
		this.gl.drawElements(this.gl.TRIANGLES, this.indexCount, this.gl.UNSIGNED_SHORT, 0);
	}

	getModelMatrix() {
		const m = glMat4.create();
		glMat4.rotateY(m, m, this.dragonRotateY);
		glMat4.translate(m, m, [0, 0, -3]);
		return m;
	}
}

const scene = new Scene({
	clearColor: [0.98, 0.98, 0.98, 1]
});
const gl = scene.gl;

const shadowMap = new ShadowMap(gl, shadowTextureSize);

const camera = new RotationCamera([0, 0, 0], Math.PI / 5.0, 0);
camera.registerEvents(scene.canvas);

const cameraShader = new Shader(gl, cameraVertexGLSL, cameraFragmentGLSL);

// We enable our vertex attributes for our camera's shader.
const vertexPositionAttrib = cameraShader.getAttributeLocation('position');

const dragon = new Dragon(gl, vertexPositionAttrib);
const floor = new Floor(gl, vertexPositionAttrib);

scene.addSceneObject(dragon, true, true);
scene.addSceneObject(floor, false, true);

cameraShader.use();
shadowMap.setVars(cameraShader);

function drawShadowMap () {
	shadowMap.start();

	dragon.bind();
	dragon.render(shadowMap, null, null);

	shadowMap.end();
}

function drawModels () {
	scene.render();

	let cameraMatrix = camera.update();

	cameraShader.use();
	dragon.render(shadowMap, cameraMatrix, cameraShader);

	floor.bind();

	cameraShader.setMatrix4fv('lightMViewMatrix', shadowMap.lightViewMatrix);
	cameraShader.setMatrix4fv('MV', cameraMatrix);
	cameraShader.set3fv('uColor', [0.6, 0.6, 0.6]);

	floor.render();
}


function draw () {
	dragon.animate();
	drawShadowMap();
	drawModels();

	window.requestAnimationFrame(draw);
}
draw();
