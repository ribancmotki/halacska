// virtual:index.js
import * as THREE4 from "three/webgpu";
import {
  Fn as Fn4,
  If as If2,
  uniform as uniform4,
  float as float4,
  vec2 as vec22,
  vec3 as vec34,
  vec4 as vec43,
  positionWorld,
  positionGeometry as positionGeometry2,
  time as time3,
  sin as sin2,
  cos as cos2,
  mix as mix2,
  smoothstep as smoothstep3,
  hash as hash2,
  mx_noise_float as mx_noise_float3,
  sqrt as sqrt2,
  fract as fract2,
  uv as uv3,
  vertexIndex,
  instanceIndex as instanceIndex2,
  cameraViewMatrix,
  rotate,
  pass,
  saturation,
  texture as texture3
} from "three/tsl";
import { OrbitControls } from "three/addons/controls/OrbitControls.js";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";
import { DRACOLoader } from "three/addons/loaders/DRACOLoader.js";

// virtual:GrassNoodles.js
import * as THREE from "three/webgpu";
import { StorageBufferAttribute } from "three/webgpu";
import {
  Fn,
  uniform,
  float,
  vec3,
  vec4,
  instancedArray,
  instanceIndex,
  If,
  Loop,
  storage,
  uv,
  texture,
  positionGeometry,
  normalGeometry,
  mix,
  smoothstep,
  sqrt,
  sin,
  cos,
  hash,
  deltaTime,
  time,
  mx_noise_float,
  length,
  max,
  min,
  normalize,
  cross,
  select,
  pow,
  clamp,
  fract,
  PI
} from "three/tsl";
var GrassNoodles = class _GrassNoodles {
  // Maximum simultaneous colliders. Each slot is either a 'push' (soft
  // radial impulse) or 'collision' (hard capsule projection); see the
  // `colliders` array on each instance. Inactive slots stay parked at
  // (99999, 0, 99999) with zero radius and strength.
  static MAX_COLLIDERS = 10;
  // Spatial grid over the blade field XZ — per-blade collider culling.
  // Each cell stores up to MAX_PER_CELL collider indices (with sentinel
  // MAX_COLLIDERS for empty slots, which dereferences to a zero-radius
  // "null" entry so per-iteration math collapses to a no-op).
  static GRID_RES = 32;
  static MAX_PER_CELL = 4;
  // Private uniform nodes
  #bladeWidthU;
  #bladeHeightU;
  #segmentLengthU;
  #stiffnessU;
  #dampingU;
  #dragU;
  #pushRadiusU;
  #pushScaleU;
  #pushRecoveryU;
  #colliderPositionsU;
  #colliderEndpointsU;
  #colliderStrengthsU;
  #colliderRadiiU;
  #colliderTypesU;
  #densityU;
  #bladeColorVariationU;
  #bladeGradientFalloffU;
  #bladeGradientOffsetU;
  #bladeBaseColorU;
  #bladeTipColorU;
  #buoyancyU;
  #gravityU;
  #tipMassU;
  #flowAmplitudeU;
  #flowFrequencyU;
  #flowSpeedU;
  #flowPhaseOffsetU;
  #flowVerticalAmplitudeU;
  #waveAmplitudeU;
  #waveSpeedU;
  #wavePhasePerJointU;
  #selfRightU;
  #tiltAmplitudeU;
  #emissiveNoiseScaleU;
  #emissiveNoiseStrengthU;
  #speckleScaleU;
  #speckleIntensityU;
  #speckleThresholdU;
  // Private state
  #renderer;
  #bladeCount;
  #jointsPerBlade;
  #segmentsPerBlade;
  #positioningNode;
  #bladeGeometry;
  #materialOverride;
  #computeUpdate;
  #computeInit;
  // Spatial grid (CPU-built, GPU-consumed each frame)
  #gridFieldSize;
  #colliderGridArr;
  #colliderGridAttr;
  #colliderGridStorage;
  #colliderDataArr;
  #colliderDataAttr;
  #colliderDataStorage;
  // Public state
  bladeData;
  bladeIsShown;
  positions;
  prevPositions;
  group = new THREE.Group();
  mesh;
  material;
  constructor(renderer2, settings = {}) {
    this.#renderer = renderer2;
    this.#bladeCount = settings.bladeCount ?? 3e4;
    this.#jointsPerBlade = settings.jointsPerBlade ?? 6;
    this.#segmentsPerBlade = this.#jointsPerBlade - 1;
    this.bladeData = instancedArray(this.#bladeCount, "vec4");
    this.bladeIsShown = instancedArray(this.#bladeCount, "float");
    const totalJoints = this.#bladeCount * this.#jointsPerBlade;
    this.positions = instancedArray(totalJoints, "vec4");
    this.prevPositions = instancedArray(totalJoints, "vec4");
    this.#bladeWidthU = uniform(settings.bladeWidth ?? 1);
    this.#bladeHeightU = uniform(settings.bladeHeight ?? 1.6);
    this.#segmentLengthU = uniform(settings.segmentLength ?? 1 / this.#segmentsPerBlade);
    this.#stiffnessU = uniform(settings.stiffness ?? 0.5);
    this.#dampingU = uniform(settings.damping ?? 0.08);
    this.#dragU = uniform(settings.drag ?? 0);
    this.#pushRadiusU = uniform(settings.pushRadius ?? 3);
    this.#pushScaleU = uniform(settings.pushScale ?? 1);
    this.#pushRecoveryU = uniform(settings.pushRecovery ?? 1.5);
    const initialStrength = settings.pushStrength ?? 1.5;
    this.#colliderPositionsU = [];
    this.#colliderEndpointsU = [];
    this.#colliderStrengthsU = [];
    this.#colliderRadiiU = [];
    this.#colliderTypesU = [];
    this.colliders = [];
    for (let i = 0; i < _GrassNoodles.MAX_COLLIDERS; i++) {
      const posU = uniform(new THREE.Vector3(99999, 0, 99999));
      const endU = uniform(new THREE.Vector3(99999, 0, 99999));
      const strU = uniform(settings.pushStrengths?.[i] ?? initialStrength);
      const radU = uniform(settings.colliderRadii?.[i] ?? 0);
      const typU = uniform(settings.colliderTypes?.[i] === "collision" ? 1 : 0);
      this.#colliderPositionsU.push(posU);
      this.#colliderEndpointsU.push(endU);
      this.#colliderStrengthsU.push(strU);
      this.#colliderRadiiU.push(radU);
      this.#colliderTypesU.push(typU);
      this.colliders.push({
        get type() {
          return typU.value === 1 ? "collision" : "push";
        },
        set type(v) {
          typU.value = v === "collision" ? 1 : 0;
        },
        get position() {
          return posU.value;
        },
        get endpoint() {
          return endU.value;
        },
        get strength() {
          return strU.value;
        },
        set strength(v) {
          strU.value = v;
        },
        get radius() {
          return radU.value;
        },
        set radius(v) {
          radU.value = v;
        }
      });
    }
    this.#buoyancyU = uniform(settings.buoyancy ?? 0);
    this.#gravityU = uniform(settings.gravity ?? 0);
    this.#tipMassU = uniform(settings.tipMass ?? 1);
    this.#flowAmplitudeU = uniform(settings.flowAmplitude ?? 0);
    this.#flowFrequencyU = uniform(settings.flowFrequency ?? 0.15);
    this.#flowSpeedU = uniform(settings.flowSpeed ?? 0.3);
    this.#flowPhaseOffsetU = uniform(settings.flowPhaseOffset ?? 0.4);
    this.#flowVerticalAmplitudeU = uniform(settings.flowVerticalAmplitude ?? 0);
    this.#waveAmplitudeU = uniform(settings.waveAmplitude ?? 0);
    this.#waveSpeedU = uniform(settings.waveSpeed ?? 0.3);
    this.#wavePhasePerJointU = uniform(settings.wavePhasePerJoint ?? 1);
    this.#selfRightU = uniform(settings.selfRight ?? 0);
    this.#tiltAmplitudeU = uniform(settings.tiltAmplitude ?? 0);
    this.#densityU = uniform(settings.density ?? 1);
    this.#bladeColorVariationU = uniform(settings.bladeColorVariation ?? 0.9);
    this.#bladeGradientFalloffU = uniform(settings.bladeGradientFalloff ?? 1.6);
    this.#bladeGradientOffsetU = uniform(settings.bladeGradientOffset ?? 0);
    this.#bladeBaseColorU = uniform(new THREE.Color(settings.bladeBaseColor ?? "#0a2030"));
    this.#bladeTipColorU = uniform(new THREE.Color(settings.bladeTipColor ?? "#3c7986"));
    this.#emissiveNoiseScaleU = uniform(settings.emissiveNoiseScale ?? 12);
    this.#emissiveNoiseStrengthU = uniform(settings.emissiveNoiseStrength ?? 0.7);
    this.#speckleScaleU = uniform(settings.speckleScale ?? 40);
    this.#speckleIntensityU = uniform(settings.speckleIntensity ?? 0.4);
    this.#speckleThresholdU = uniform(settings.speckleThreshold ?? 0.7);
    this.#positioningNode = settings.positioningNode;
    this.#bladeGeometry = settings.bladeGeometry;
    this.#materialOverride = settings.material;
    this.#gridFieldSize = settings.gridFieldSize ?? 24;
    const cellCount = _GrassNoodles.GRID_RES * _GrassNoodles.GRID_RES;
    const totalGridSlots = cellCount * _GrassNoodles.MAX_PER_CELL;
    this.#colliderGridArr = new Uint32Array(totalGridSlots).fill(_GrassNoodles.MAX_COLLIDERS);
    this.#colliderGridAttr = new StorageBufferAttribute(this.#colliderGridArr, 1);
    this.#colliderGridStorage = storage(this.#colliderGridAttr, "uint", totalGridSlots);
    const dataEntries = _GrassNoodles.MAX_COLLIDERS + 1;
    this.#colliderDataArr = new Float32Array(dataEntries * 3 * 4);
    this.#colliderDataAttr = new StorageBufferAttribute(this.#colliderDataArr, 4);
    this.#colliderDataStorage = storage(this.#colliderDataAttr, "vec4", dataEntries * 3);
  }
  // ─── Spatial grid rebuild (CPU → storage buffer) ──────────────────────────
  #rebuildColliderGrid() {
    const NULL_IDX = _GrassNoodles.MAX_COLLIDERS;
    const data = this.#colliderDataArr;
    const grid = this.#colliderGridArr;
    for (let c = 0; c < _GrassNoodles.MAX_COLLIDERS; c++) {
      const off = c * 12;
      const pos = this.#colliderPositionsU[c].value;
      const end = this.#colliderEndpointsU[c].value;
      data[off + 0] = pos.x;
      data[off + 1] = pos.y;
      data[off + 2] = pos.z;
      data[off + 3] = this.#colliderRadiiU[c].value;
      data[off + 4] = end.x;
      data[off + 5] = end.y;
      data[off + 6] = end.z;
      data[off + 7] = this.#colliderStrengthsU[c].value;
      data[off + 8] = this.#colliderTypesU[c].value;
      data[off + 9] = 0;
      data[off + 10] = 0;
      data[off + 11] = 0;
    }
    const nullOff = NULL_IDX * 12;
    data[nullOff + 0] = 99999;
    data[nullOff + 1] = 0;
    data[nullOff + 2] = 99999;
    data[nullOff + 3] = 0;
    data[nullOff + 4] = 99999;
    data[nullOff + 5] = 0;
    data[nullOff + 6] = 99999;
    data[nullOff + 7] = 0;
    data[nullOff + 8] = 0;
    data[nullOff + 9] = 0;
    data[nullOff + 10] = 0;
    data[nullOff + 11] = 0;
    this.#colliderDataAttr.needsUpdate = true;
    grid.fill(NULL_IDX);
    const RES = _GrassNoodles.GRID_RES;
    const MAX_PER_CELL = _GrassNoodles.MAX_PER_CELL;
    const fieldSize = this.#gridFieldSize;
    const half = fieldSize / 2;
    const cellSize = fieldSize / RES;
    const pushRadius = this.#pushRadiusU.value;
    const margin = this.#bladeHeightU.value;
    for (let c = 0; c < _GrassNoodles.MAX_COLLIDERS; c++) {
      const pos = this.#colliderPositionsU[c].value;
      const end = this.#colliderEndpointsU[c].value;
      const radius = this.#colliderRadiiU[c].value;
      const type = this.#colliderTypesU[c].value;
      const isCollision = type === 1;
      const reach = isCollision ? radius : pushRadius;
      if (reach <= 0) continue;
      if (Math.abs(pos.x) > 9e3 || Math.abs(pos.z) > 9e3) continue;
      let minX, maxX, minZ, maxZ;
      if (isCollision) {
        minX = Math.min(pos.x, end.x) - radius;
        maxX = Math.max(pos.x, end.x) + radius;
        minZ = Math.min(pos.z, end.z) - radius;
        maxZ = Math.max(pos.z, end.z) + radius;
      } else {
        minX = pos.x - pushRadius;
        maxX = pos.x + pushRadius;
        minZ = pos.z - pushRadius;
        maxZ = pos.z + pushRadius;
      }
      minX -= margin;
      maxX += margin;
      minZ -= margin;
      maxZ += margin;
      if (maxX < -half || minX > half || maxZ < -half || minZ > half) continue;
      const cxMin = Math.max(0, Math.floor((minX + half) / cellSize));
      const cxMax = Math.min(RES - 1, Math.floor((maxX + half) / cellSize));
      const czMin = Math.max(0, Math.floor((minZ + half) / cellSize));
      const czMax = Math.min(RES - 1, Math.floor((maxZ + half) / cellSize));
      for (let cz = czMin; cz <= czMax; cz++) {
        for (let cx = cxMin; cx <= cxMax; cx++) {
          const baseIdx = (cz * RES + cx) * MAX_PER_CELL;
          for (let k = 0; k < MAX_PER_CELL; k++) {
            if (grid[baseIdx + k] === NULL_IDX) {
              grid[baseIdx + k] = c;
              break;
            }
          }
        }
      }
    }
    this.#colliderGridAttr.needsUpdate = true;
  }
  // ─── Property getters/setters ──────────────────────────────────────────────
  get jointsPerBlade() {
    return this.#jointsPerBlade;
  }
  get bladeCount() {
    return this.#bladeCount;
  }
  get bladeWidth() {
    return this.#bladeWidthU.value;
  }
  set bladeWidth(v) {
    this.#bladeWidthU.value = v;
  }
  get bladeHeight() {
    return this.#bladeHeightU.value;
  }
  set bladeHeight(v) {
    this.#bladeHeightU.value = v;
  }
  get stiffness() {
    return this.#stiffnessU.value;
  }
  set stiffness(v) {
    this.#stiffnessU.value = v;
  }
  get damping() {
    return this.#dampingU.value;
  }
  set damping(v) {
    this.#dampingU.value = v;
  }
  get drag() {
    return this.#dragU.value;
  }
  set drag(v) {
    this.#dragU.value = v;
  }
  get pushRadius() {
    return this.#pushRadiusU.value;
  }
  set pushRadius(v) {
    this.#pushRadiusU.value = v;
  }
  get pushScale() {
    return this.#pushScaleU.value;
  }
  set pushScale(v) {
    this.#pushScaleU.value = v;
  }
  get pushRecovery() {
    return this.#pushRecoveryU.value;
  }
  set pushRecovery(v) {
    this.#pushRecoveryU.value = v;
  }
  get buoyancy() {
    return this.#buoyancyU.value;
  }
  set buoyancy(v) {
    this.#buoyancyU.value = v;
  }
  get gravity() {
    return this.#gravityU.value;
  }
  set gravity(v) {
    this.#gravityU.value = v;
  }
  get tipMass() {
    return this.#tipMassU.value;
  }
  set tipMass(v) {
    this.#tipMassU.value = v;
  }
  get flowAmplitude() {
    return this.#flowAmplitudeU.value;
  }
  set flowAmplitude(v) {
    this.#flowAmplitudeU.value = v;
  }
  get flowFrequency() {
    return this.#flowFrequencyU.value;
  }
  set flowFrequency(v) {
    this.#flowFrequencyU.value = v;
  }
  get flowSpeed() {
    return this.#flowSpeedU.value;
  }
  set flowSpeed(v) {
    this.#flowSpeedU.value = v;
  }
  get flowPhaseOffset() {
    return this.#flowPhaseOffsetU.value;
  }
  set flowPhaseOffset(v) {
    this.#flowPhaseOffsetU.value = v;
  }
  get flowVerticalAmplitude() {
    return this.#flowVerticalAmplitudeU.value;
  }
  set flowVerticalAmplitude(v) {
    this.#flowVerticalAmplitudeU.value = v;
  }
  get waveAmplitude() {
    return this.#waveAmplitudeU.value;
  }
  set waveAmplitude(v) {
    this.#waveAmplitudeU.value = v;
  }
  get waveSpeed() {
    return this.#waveSpeedU.value;
  }
  set waveSpeed(v) {
    this.#waveSpeedU.value = v;
  }
  get wavePhasePerJoint() {
    return this.#wavePhasePerJointU.value;
  }
  set wavePhasePerJoint(v) {
    this.#wavePhasePerJointU.value = v;
  }
  get selfRight() {
    return this.#selfRightU.value;
  }
  set selfRight(v) {
    this.#selfRightU.value = v;
  }
  get tiltAmplitude() {
    return this.#tiltAmplitudeU.value;
  }
  set tiltAmplitude(v) {
    this.#tiltAmplitudeU.value = v;
  }
  get density() {
    return this.#densityU.value;
  }
  set density(v) {
    this.#densityU.value = v;
  }
  get bladeColorVariation() {
    return this.#bladeColorVariationU.value;
  }
  set bladeColorVariation(v) {
    this.#bladeColorVariationU.value = v;
  }
  get bladeGradientFalloff() {
    return this.#bladeGradientFalloffU.value;
  }
  set bladeGradientFalloff(v) {
    this.#bladeGradientFalloffU.value = v;
  }
  get bladeGradientOffset() {
    return this.#bladeGradientOffsetU.value;
  }
  set bladeGradientOffset(v) {
    this.#bladeGradientOffsetU.value = v;
  }
  get bladeBaseColor() {
    return `#${this.#bladeBaseColorU.value.getHexString()}`;
  }
  set bladeBaseColor(v) {
    this.#bladeBaseColorU.value.set(v);
  }
  get bladeTipColor() {
    return `#${this.#bladeTipColorU.value.getHexString()}`;
  }
  set bladeTipColor(v) {
    this.#bladeTipColorU.value.set(v);
  }
  get emissiveNoiseStrength() {
    return this.#emissiveNoiseStrengthU.value;
  }
  set emissiveNoiseStrength(v) {
    this.#emissiveNoiseStrengthU.value = v;
  }
  get emissiveNoiseScale() {
    return this.#emissiveNoiseScaleU.value;
  }
  set emissiveNoiseScale(v) {
    this.#emissiveNoiseScaleU.value = v;
  }
  get speckleScale() {
    return this.#speckleScaleU.value;
  }
  set speckleScale(v) {
    this.#speckleScaleU.value = v;
  }
  get speckleIntensity() {
    return this.#speckleIntensityU.value;
  }
  set speckleIntensity(v) {
    this.#speckleIntensityU.value = v;
  }
  get speckleThreshold() {
    return this.#speckleThresholdU.value;
  }
  set speckleThreshold(v) {
    this.#speckleThresholdU.value = v;
  }
  get computeInit() {
    return this.#computeInit;
  }
  // ─── Init ──────────────────────────────────────────────────────────────────
  init() {
    this.#createMaterial();
    this.#createMesh();
    this.#createComputeInit();
    this.#createComputeUpdate();
    return this;
  }
  update() {
    this.#rebuildColliderGrid();
    this.#renderer.compute(this.#computeUpdate);
  }
  // ─── Base geometry ─────────────────────────────────────────────────────────
  #createBladeGeometry() {
    const r = 0.05;
    const h = 1 - 2 * r;
    const geo = new THREE.CapsuleGeometry(r, h, 6, 8, 8);
    geo.translate(0, 0.5, 0);
    return geo;
  }
  // ─── Material (vertex shader follows the joint chain) ─────────────────────
  #createMaterial() {
    this.material = this.#materialOverride ?? new THREE.MeshStandardNodeMaterial({
      side: THREE.FrontSide,
      roughness: 0.75,
      metalness: 0.05
    });
    const positions = this.positions;
    const bladeData = this.bladeData;
    const bladeIsShown = this.bladeIsShown;
    const jointsPerBlade2 = this.#jointsPerBlade;
    const segmentsPerBlade = this.#segmentsPerBlade;
    const bladeWidthU = this.#bladeWidthU;
    this.material.positionNode = Fn(() => {
      const shown = bladeIsShown.element(instanceIndex);
      const t = positionGeometry.y.saturate().mul(float(segmentsPerBlade));
      const segF = t.floor().min(float(segmentsPerBlade - 1));
      const segT = t.sub(segF);
      const segI = segF.toInt();
      const baseJointIdx = instanceIndex.mul(jointsPerBlade2);
      const jointAIdx = baseJointIdx.add(segI);
      const jointBIdx = jointAIdx.add(1);
      const jointPrevIdx = baseJointIdx.add(segI.sub(1).max(0));
      const jointNextIdx = baseJointIdx.add(segI.add(2).min(jointsPerBlade2 - 1));
      const jointA = positions.element(jointAIdx).xyz;
      const jointB = positions.element(jointBIdx).xyz;
      const jointPrev = positions.element(jointPrevIdx).xyz;
      const jointNext = positions.element(jointNextIdx).xyz;
      const spine = mix(jointA, jointB, segT);
      const tangentA = normalize(jointB.sub(jointPrev));
      const tangentB = normalize(jointNext.sub(jointA));
      const segDir = normalize(mix(tangentA, tangentB, segT));
      const ref = vec3(1, 0, 0);
      const right = normalize(cross(segDir, ref));
      const fwd = normalize(cross(right, segDir));
      const radialScale = bladeWidthU;
      const offset = right.mul(positionGeometry.x.mul(radialScale)).add(fwd.mul(positionGeometry.z.mul(radialScale)));
      const finalPos = spine.add(offset);
      return finalPos.mul(shown);
    })();
    this.material.normalNode = Fn(() => {
      const t = positionGeometry.y.saturate().mul(float(segmentsPerBlade));
      const segF = t.floor().min(float(segmentsPerBlade - 1));
      const segT = t.sub(segF);
      const segI = segF.toInt();
      const baseJointIdx = instanceIndex.mul(jointsPerBlade2);
      const jointAIdx = baseJointIdx.add(segI);
      const jointBIdx = jointAIdx.add(1);
      const jointPrevIdx = baseJointIdx.add(segI.sub(1).max(0));
      const jointNextIdx = baseJointIdx.add(segI.add(2).min(jointsPerBlade2 - 1));
      const jointA = positions.element(jointAIdx).xyz;
      const jointB = positions.element(jointBIdx).xyz;
      const jointPrev = positions.element(jointPrevIdx).xyz;
      const jointNext = positions.element(jointNextIdx).xyz;
      const tangentA = normalize(jointB.sub(jointPrev));
      const tangentB = normalize(jointNext.sub(jointA));
      const segDir = normalize(mix(tangentA, tangentB, segT));
      const ref = vec3(1, 0, 0);
      const right = normalize(cross(segDir, ref));
      const fwd = normalize(cross(right, segDir));
      const n = normalGeometry;
      return normalize(right.mul(n.x).add(segDir.mul(n.y)).add(fwd.mul(n.z)));
    })();
    this.material.colorNode = Fn(() => {
      const t = positionGeometry.y.saturate();
      const clump = bladeData.element(instanceIndex).w.saturate();
      const shifted = t.sub(this.#bladeGradientOffsetU).div(float(1).sub(this.#bladeGradientOffsetU).max(1e-3)).clamp(0, 1);
      const gradient = pow(shifted, this.#bladeGradientFalloffU);
      const tipMix = float(1).sub(this.#bladeColorVariationU).add(clump.mul(this.#bladeColorVariationU));
      const variedTip = mix(this.#bladeBaseColorU, this.#bladeTipColorU, tipMix);
      return mix(this.#bladeBaseColorU, variedTip, gradient);
    })();
    if (this.material.emissiveMap) {
      const emissiveMap = this.material.emissiveMap;
      const emissiveColorU = uniform(this.material.emissive);
      const emissiveIntensityU = uniform(this.material.emissiveIntensity);
      this._emissiveColorU = emissiveColorU;
      this._emissiveIntensityU = emissiveIntensityU;
      this.material.emissiveNode = Fn(() => {
        const mask = texture(emissiveMap, uv()).r;
        const t2 = positionGeometry.y.saturate().mul(float(segmentsPerBlade));
        const segF2 = t2.floor().min(float(segmentsPerBlade - 1));
        const segT2 = t2.sub(segF2);
        const segI2 = segF2.toInt();
        const baseJointIdx2 = instanceIndex.mul(jointsPerBlade2);
        const jointA2 = positions.element(baseJointIdx2.add(segI2)).xyz;
        const jointB2 = positions.element(baseJointIdx2.add(segI2).add(1)).xyz;
        const spine2 = mix(jointA2, jointB2, segT2);
        const bladeHash2 = hash(instanceIndex).mul(100);
        const emNoiseStrU = this.#emissiveNoiseStrengthU;
        const noiseCoord2 = spine2.mul(this.#emissiveNoiseScaleU).add(vec3(bladeHash2, float(0), bladeHash2.mul(0.7)));
        const rawNoise = mx_noise_float(noiseCoord2.add(vec3(113.7, 47.3, 83.1))).mul(0.5).add(0.5);
        const noiseVal = mix(float(1), rawNoise, emNoiseStrU);
        const cellCoord = spine2.mul(this.#speckleScaleU).add(vec3(bladeHash2.mul(1.3), bladeHash2.mul(0.9), bladeHash2.mul(1.7)));
        const cellSeed = fract(cellCoord.x).mul(12.9898).add(fract(cellCoord.y).mul(78.233)).add(fract(cellCoord.z).mul(45.164));
        const speckleRaw = fract(sin(cellSeed).mul(43758.5453));
        const speckleMask = smoothstep(this.#speckleThresholdU, this.#speckleThresholdU.add(0.05), speckleRaw);
        const inverseMask = float(1).sub(mask).clamp(0, 1);
        const speckle = speckleMask.mul(inverseMask).mul(this.#speckleIntensityU);
        return emissiveColorU.mul(emissiveIntensityU).mul(mask).mul(noiseVal).add(emissiveColorU.mul(speckle));
      })();
    }
  }
  // ─── Mesh ──────────────────────────────────────────────────────────────────
  #createMesh() {
    const bladeGeo = this.#bladeGeometry ?? this.#createBladeGeometry();
    this.mesh = new THREE.InstancedMesh(bladeGeo, this.material, this.#bladeCount);
    this.mesh.frustumCulled = false;
    const dummy = new THREE.Object3D();
    for (let i = 0; i < this.#bladeCount; i++) this.mesh.setMatrixAt(i, dummy.matrix);
    this.mesh.instanceMatrix.needsUpdate = true;
    this.group.add(this.mesh);
  }
  // ─── Compute: init blade data + joint positions ───────────────────────────
  #createComputeInit() {
    const positioningNode = this.#positioningNode;
    const bladeData = this.bladeData;
    const bladeIsShown = this.bladeIsShown;
    const positions = this.positions;
    const prevPositions = this.prevPositions;
    const jointsPerBlade2 = this.#jointsPerBlade;
    const segmentLengthU = this.#segmentLengthU;
    const bladeHeightU = this.#bladeHeightU;
    const densityU = this.#densityU;
    const tiltAmplitudeU = this.#tiltAmplitudeU;
    this.#computeInit = Fn(() => {
      const bladeIdx = instanceIndex;
      const { wx, wz, isShown = float(1) } = positioningNode(bladeIdx);
      const densityMask = hash(bladeIdx.add(5381)).lessThan(densityU.min(1)).select(float(1), float(0));
      const finalShown = isShown.mul(densityMask).greaterThanEqual(0.5).select(float(1), float(0));
      bladeIsShown.element(bladeIdx).assign(finalShown);
      const n1 = mx_noise_float(vec3(wx.mul(0.35), float(0), wz.mul(0.35))).mul(0.5).add(0.5);
      const n2 = mx_noise_float(vec3(wx.mul(1.7).add(50), float(7), wz.mul(1.7).add(50))).mul(0.5).add(0.5);
      const clump = n1.mul(0.5).add(n2.mul(0.25)).max(0.6);
      bladeData.element(bladeIdx).assign(vec4(wx, wz, float(0), clump));
      const tiltAngle = hash(bladeIdx.add(7)).mul(PI).mul(2);
      const tiltMag = hash(bladeIdx.add(13)).mul(tiltAmplitudeU);
      const tiltX = cos(tiltAngle).mul(tiltMag);
      const tiltZ = sin(tiltAngle).mul(tiltMag);
      const restDir = normalize(vec3(tiltX, float(1), tiltZ));
      const heightScale = float(0.4).add(clump).mul(finalShown.sign());
      const segLen = segmentLengthU.mul(bladeHeightU).mul(heightScale);
      const root = vec3(wx, float(0), wz);
      for (let i = 0; i < jointsPerBlade2; i++) {
        const jointIdx = bladeIdx.mul(jointsPerBlade2).add(i);
        const pos = root.add(restDir.mul(segLen.mul(float(i))));
        positions.element(jointIdx).assign(vec4(pos, float(0)));
        prevPositions.element(jointIdx).assign(vec4(pos, float(0)));
      }
    })().compute(this.#bladeCount);
  }
  // ─── Compute: per-frame simulation (one thread per blade) ─────────────────
  #createComputeUpdate() {
    const positioningNode = this.#positioningNode;
    const positions = this.positions;
    const prevPositions = this.prevPositions;
    const bladeData = this.bladeData;
    const bladeIsShown = this.bladeIsShown;
    const jointsPerBlade2 = this.#jointsPerBlade;
    const segmentsPerBlade = this.#segmentsPerBlade;
    const segmentLengthU = this.#segmentLengthU;
    const bladeHeightU = this.#bladeHeightU;
    const stiffnessU = this.#stiffnessU;
    const dampingU = this.#dampingU;
    const dragU = this.#dragU;
    const pushRadiusU = this.#pushRadiusU;
    const pushScaleU = this.#pushScaleU;
    const pushRecoveryU = this.#pushRecoveryU;
    const colliderGridStorage = this.#colliderGridStorage;
    const colliderDataStorage = this.#colliderDataStorage;
    const GRID_RES = _GrassNoodles.GRID_RES;
    const MAX_PER_CELL = _GrassNoodles.MAX_PER_CELL;
    const halfFieldF = this.#gridFieldSize / 2;
    const cellSizeF = this.#gridFieldSize / _GrassNoodles.GRID_RES;
    const buoyancyU = this.#buoyancyU;
    const gravityU = this.#gravityU;
    const tipMassU = this.#tipMassU;
    const flowAmplitudeU = this.#flowAmplitudeU;
    const flowFrequencyU = this.#flowFrequencyU;
    const flowSpeedU = this.#flowSpeedU;
    const flowPhaseOffsetU = this.#flowPhaseOffsetU;
    const flowVerticalAmplitudeU = this.#flowVerticalAmplitudeU;
    const waveAmplitudeU = this.#waveAmplitudeU;
    const waveSpeedU = this.#waveSpeedU;
    const wavePhasePerJointU = this.#wavePhasePerJointU;
    const selfRightU = this.#selfRightU;
    const tiltAmplitudeU = this.#tiltAmplitudeU;
    this.#computeUpdate = Fn(() => {
      const bladeIdx = instanceIndex;
      If(bladeIsShown.element(bladeIdx).greaterThan(0), () => {
        const { wx, wz } = positioningNode(bladeIdx);
        const blade = bladeData.element(bladeIdx);
        const heightScale = float(0.4).add(blade.w);
        const segLen = segmentLengthU.mul(bladeHeightU).mul(heightScale);
        const tiltAngle = hash(bladeIdx.add(7)).mul(PI).mul(2);
        const tiltMag = hash(bladeIdx.add(13)).mul(tiltAmplitudeU);
        const restTiltX = cos(tiltAngle).mul(tiltMag);
        const restTiltZ = sin(tiltAngle).mul(tiltMag);
        const restDir = normalize(vec3(restTiltX, float(1), restTiltZ));
        const rootIdx = bladeIdx.mul(jointsPerBlade2);
        const rootPos = vec3(wx, float(0), wz);
        positions.element(rootIdx).assign(vec4(rootPos, float(0)));
        prevPositions.element(rootIdx).assign(vec4(rootPos, float(0)));
        const cellX = wx.add(halfFieldF).div(cellSizeF).floor().toInt().max(0).min(GRID_RES - 1);
        const cellZ = wz.add(halfFieldF).div(cellSizeF).floor().toInt().max(0).min(GRID_RES - 1);
        const cellBase = cellZ.mul(GRID_RES).add(cellX).mul(MAX_PER_CELL);
        const dt = deltaTime;
        const oneMinusDamp = float(1).sub(dampingU);
        const flowPhaseX = hash(bladeIdx.add(23)).sub(0.5).mul(2).mul(flowPhaseOffsetU);
        const flowPhaseZ = hash(bladeIdx.add(41)).sub(0.5).mul(2).mul(flowPhaseOffsetU);
        const waveAngle = hash(bladeIdx.add(53)).mul(PI).mul(2);
        const waveDirX = cos(waveAngle);
        const waveDirZ = sin(waveAngle);
        const waveBladeOffset = hash(bladeIdx.add(67)).mul(PI).mul(2);
        for (let i = 1; i < jointsPerBlade2; i++) {
          const jointIdx = bladeIdx.mul(jointsPerBlade2).add(i);
          const pos = positions.element(jointIdx).xyz;
          const prev = prevPositions.element(jointIdx).xyz;
          const velLin = pos.sub(prev).mul(oneMinusDamp);
          const speed = length(velLin);
          const vel = velLin.div(float(1).add(dragU.mul(speed)));
          const heightFactor = pow(float(i).div(segmentsPerBlade), float(1.4));
          const pushX = float(0).toVar();
          const pushZ = float(0).toVar();
          Loop(MAX_PER_CELL, ({ i: k }) => {
            const collIdx = colliderGridStorage.element(cellBase.add(k));
            const dataBase = collIdx.mul(3);
            const v0 = colliderDataStorage.element(dataBase);
            const v1 = colliderDataStorage.element(dataBase.add(1));
            const v2 = colliderDataStorage.element(dataBase.add(2));
            const cPos = v0.xyz;
            const cStrength = v1.w;
            const cType = v2.x;
            const isPush = float(1).sub(cType);
            const dx = pos.x.sub(cPos.x);
            const dz = pos.z.sub(cPos.z);
            const distH = sqrt(dx.mul(dx).add(dz.mul(dz))).add(1e-4);
            const falloff = float(1).sub(clamp(distH.div(pushRadiusU), float(0), float(1)));
            const influence = falloff.mul(falloff).mul(cStrength).mul(pushScaleU).mul(heightFactor).mul(dt).mul(isPush);
            pushX.addAssign(dx.div(distH).mul(influence));
            pushZ.addAssign(dz.div(distH).mul(influence));
          });
          const tipFactor = pow(float(i).div(segmentsPerBlade), tipMassU);
          const vyBias = buoyancyU.mul(heightFactor).sub(gravityU.mul(tipFactor)).mul(dt);
          const eps = float(0.5);
          const xs = pos.x.mul(flowFrequencyU).add(flowPhaseX);
          const zs = pos.z.mul(flowFrequencyU).add(flowPhaseZ);
          const ts = time.mul(flowSpeedU);
          const phiXpos = mx_noise_float(vec3(xs.add(eps), zs, ts));
          const phiXneg = mx_noise_float(vec3(xs.sub(eps), zs, ts));
          const phiZpos = mx_noise_float(vec3(xs, zs.add(eps), ts));
          const phiZneg = mx_noise_float(vec3(xs, zs.sub(eps), ts));
          const flowAmpStep = flowAmplitudeU.mul(heightFactor).mul(dt);
          const flowX = phiZpos.sub(phiZneg).mul(flowAmpStep);
          const flowZ = phiXneg.sub(phiXpos).mul(flowAmpStep);
          const phiY = mx_noise_float(vec3(xs.add(100), zs.add(100), ts.mul(0.7)));
          const flowY = phiY.mul(flowVerticalAmplitudeU).mul(heightFactor).mul(dt);
          const wavePhase = time.mul(waveSpeedU).mul(PI.mul(2)).sub(float(i).mul(wavePhasePerJointU)).add(waveBladeOffset);
          const waveStep = sin(wavePhase).mul(waveAmplitudeU).mul(heightFactor).mul(dt);
          const waveX = waveDirX.mul(waveStep);
          const waveZ = waveDirZ.mul(waveStep);
          const newPos = vec3(
            pos.x.add(vel.x).add(pushX).add(flowX).add(waveX),
            pos.y.add(vel.y).add(vyBias).add(flowY),
            pos.z.add(vel.z).add(pushZ).add(flowZ).add(waveZ)
          );
          prevPositions.element(jointIdx).assign(vec4(pos, float(0)));
          positions.element(jointIdx).assign(vec4(newPos, float(0)));
        }
        for (let i = 1; i < jointsPerBlade2; i++) {
          const jointIdx = bladeIdx.mul(jointsPerBlade2).add(i);
          const prevIdx = bladeIdx.mul(jointsPerBlade2).add(i - 1);
          const pos = positions.element(jointIdx).xyz;
          const posPrev = positions.element(prevIdx).xyz;
          const diff = pos.sub(posPrev);
          const d = length(diff).add(1e-6);
          const dir = diff.div(d);
          const target = posPrev.add(dir.mul(segLen));
          positions.element(jointIdx).assign(vec4(target, float(0)));
        }
        const bendStep = clamp(stiffnessU.mul(dt).mul(8), float(0), float(1));
        let prevDir = restDir;
        for (let i = 0; i < segmentsPerBlade; i++) {
          const aIdx = bladeIdx.mul(jointsPerBlade2).add(i);
          const bIdx = aIdx.add(1);
          const a = positions.element(aIdx).xyz;
          const b = positions.element(bIdx).xyz;
          const diff = b.sub(a);
          const d = length(diff).add(1e-6);
          const actualDir = diff.div(d);
          const targetDir = normalize(mix(actualDir, prevDir, bendStep));
          positions.element(bIdx).assign(vec4(a.add(targetDir.mul(segLen)), float(0)));
          prevDir = actualDir;
        }
        const basePos = vec3(wx, float(0), wz);
        const distGate = float(1).toVar();
        Loop(MAX_PER_CELL, ({ i: k }) => {
          const collIdx = colliderGridStorage.element(cellBase.add(k));
          const cPos = colliderDataStorage.element(collIdx.mul(3)).xyz;
          const d = clamp(length(basePos.sub(cPos)).div(pushRadiusU), float(0), float(1));
          distGate.assign(min(distGate, d));
        });
        const relaxBendStep = clamp(distGate.mul(pushRecoveryU).mul(dt).mul(2.4), float(0), float(1));
        let relaxPrevDir = restDir;
        for (let i = 0; i < segmentsPerBlade; i++) {
          const aIdx = bladeIdx.mul(jointsPerBlade2).add(i);
          const bIdx = aIdx.add(1);
          const a = positions.element(aIdx).xyz;
          const b = positions.element(bIdx).xyz;
          const diff = b.sub(a);
          const d = length(diff).add(1e-6);
          const actualDir = diff.div(d);
          const targetDir = normalize(mix(actualDir, relaxPrevDir, relaxBendStep));
          positions.element(bIdx).assign(vec4(a.add(targetDir.mul(segLen)), float(0)));
          relaxPrevDir = actualDir;
        }
        let comSum = vec3(0, 0, 0);
        for (let i = 1; i < jointsPerBlade2; i++) {
          const jointIdx = bladeIdx.mul(jointsPerBlade2).add(i);
          comSum = comSum.add(positions.element(jointIdx).xyz);
        }
        const comAvg = comSum.div(float(jointsPerBlade2 - 1));
        const chainDir = normalize(comAvg.sub(rootPos));
        const selfRightStep = clamp(selfRightU.mul(dt).mul(4), float(0), float(1));
        const targetChainDir = normalize(mix(chainDir, restDir, selfRightStep));
        const cosTheta = chainDir.dot(targetChainDir);
        const rotN = cross(chainDir, targetChainDir);
        const oneOverOnePlusCos = float(1).div(cosTheta.add(1).max(1e-4));
        for (let i = 1; i < jointsPerBlade2; i++) {
          const jointIdx = bladeIdx.mul(jointsPerBlade2).add(i);
          const local = positions.element(jointIdx).xyz.sub(rootPos);
          const rotated = local.mul(cosTheta).add(cross(rotN, local)).add(rotN.mul(rotN.dot(local)).mul(oneOverOnePlusCos));
          positions.element(jointIdx).assign(vec4(rotated.add(rootPos), float(0)));
        }
        for (let i = 1; i < jointsPerBlade2; i++) {
          const jointIdx = bladeIdx.mul(jointsPerBlade2).add(i);
          const p = positions.element(jointIdx).xyz.toVar();
          Loop(MAX_PER_CELL, ({ i: k }) => {
            const collIdx = colliderGridStorage.element(cellBase.add(k));
            const dataBase = collIdx.mul(3);
            const v0 = colliderDataStorage.element(dataBase);
            const v1 = colliderDataStorage.element(dataBase.add(1));
            const v2 = colliderDataStorage.element(dataBase.add(2));
            const capA = v0.xyz;
            const radius = v0.w;
            const capB = v1.xyz;
            const isCollision = v2.x;
            const ab = capB.sub(capA);
            const ap = p.sub(capA);
            const abLen2 = ab.dot(ab).add(1e-6);
            const tParam = clamp(ap.dot(ab).div(abLen2), float(0), float(1));
            const closest = capA.add(ab.mul(tParam));
            const toP = p.sub(closest);
            const dist = length(toP).add(1e-6);
            const outDir = toP.div(dist);
            const overlap = max(float(0), radius.sub(dist)).mul(isCollision);
            p.assign(p.add(outDir.mul(overlap)));
          });
          positions.element(jointIdx).assign(vec4(p, float(0)));
        }
      });
    })().compute(this.#bladeCount);
  }
};

// virtual:ProceduralCaustics.js
import * as THREE2 from "three/webgpu";
import {
  Fn as Fn2,
  uniform as uniform2,
  vec3 as vec32,
  vec4 as vec42,
  float as float2,
  uv as uv2,
  time as time2,
  abs,
  pow as pow2,
  mx_noise_float as mx_noise_float2,
  smoothstep as smoothstep2
} from "three/tsl";
var ProceduralCaustics = class {
  #renderer;
  #renderTarget;
  #scene;
  #camera;
  #quad;
  constructor(renderer2, {
    resolution = 512,
    color = "#ffffff",
    strength = 1,
    frequency = 4,
    speed = 0.5,
    baseBrightness = 0.5
  } = {}) {
    this.#renderer = renderer2;
    const colorU = uniform2(new THREE2.Color(color));
    const strengthU = uniform2(strength);
    const frequencyU = uniform2(frequency);
    const speedU = uniform2(speed);
    const baseU = uniform2(baseBrightness);
    this.uniforms = {
      color: colorU,
      strength: strengthU,
      frequency: frequencyU,
      speed: speedU,
      baseBrightness: baseU
    };
    this.#renderTarget = new THREE2.RenderTarget(resolution, resolution, {
      type: THREE2.HalfFloatType,
      depthBuffer: false,
      stencilBuffer: false,
      generateMipmaps: false,
      colorSpace: THREE2.SRGBColorSpace
    });
    this.#renderTarget.texture.wrapS = THREE2.ClampToEdgeWrapping;
    this.#renderTarget.texture.wrapT = THREE2.ClampToEdgeWrapping;
    this.#renderTarget.texture.minFilter = THREE2.LinearFilter;
    this.#renderTarget.texture.magFilter = THREE2.LinearFilter;
    this.#renderTarget.texture.anisotropy = 8;
    const causticMat = new THREE2.MeshBasicNodeMaterial();
    causticMat.colorNode = Fn2(() => {
      const t = time2.mul(speedU);
      const p = uv2().sub(0.5).mul(frequencyU);
      const n1 = mx_noise_float2(vec32(p.x.mul(2).add(t), p.y.mul(2), t.mul(0.5)));
      const n2 = mx_noise_float2(
        vec32(p.x.mul(2.5).sub(t.mul(0.3)), p.y.mul(2.5).add(t.mul(0.5)), t.mul(0.4))
      );
      const c = abs(n1).add(abs(n2)).mul(0.5);
      const peaks = pow2(float2(1).sub(c).max(0), 8).mul(strengthU);
      const dist = uv2().sub(0.5).length().mul(2);
      const vignette = float2(1).sub(smoothstep2(0.7, 1, dist));
      const intensity = baseU.add(peaks).mul(vignette);
      return vec42(colorU.mul(intensity), 1);
    })();
    const geo = new THREE2.PlaneGeometry(2, 2);
    this.#quad = new THREE2.Mesh(geo, causticMat);
    this.#quad.frustumCulled = false;
    this.#scene = new THREE2.Scene();
    this.#scene.add(this.#quad);
    this.#camera = new THREE2.OrthographicCamera(-1, 1, 1, -1, 0, 1);
  }
  get texture() {
    return this.#renderTarget.texture;
  }
  update() {
    const prevTarget = this.#renderer.getRenderTarget();
    this.#renderer.setRenderTarget(this.#renderTarget);
    this.#renderer.render(this.#scene, this.#camera);
    this.#renderer.setRenderTarget(prevTarget);
  }
  dispose() {
    this.#renderTarget.dispose();
    this.#quad.geometry.dispose();
    this.#quad.material.dispose();
  }
};

// virtual:PathFollow.js
import * as THREE3 from "three/webgpu";
import {
  Fn as Fn3,
  uniform as uniform3,
  vec2,
  vec3 as vec33,
  float as float3,
  select as select2,
  texture as texture2,
  positionLocal,
  normalLocal,
  cross as cross2
} from "three/tsl";
var PathFollow = class {
  #samples;
  #curve;
  #dataTexture;
  constructor({ points, samples = 256, closed = false, flow = true } = {}) {
    this.#samples = samples;
    this.#curve = new THREE3.CatmullRomCurve3(points, closed);
    this.#dataTexture = this.#buildDataTexture();
    this.pathOffset = uniform3(0);
    this.pathSegment = uniform3(1);
    this.spineOffset = uniform3(0);
    this.spineLength = uniform3(1);
    this.flow = uniform3(flow ? 1 : 0);
  }
  get curve() {
    return this.#curve;
  }
  get dataTexture() {
    return this.#dataTexture;
  }
  #buildDataTexture() {
    const N = this.#samples;
    const data = new Uint16Array(N * 2 * 4);
    const tex = new THREE3.DataTexture(data, N, 2, THREE3.RGBAFormat, THREE3.HalfFloatType);
    tex.wrapS = THREE3.RepeatWrapping;
    tex.wrapT = THREE3.ClampToEdgeWrapping;
    tex.minFilter = THREE3.LinearFilter;
    tex.magFilter = THREE3.LinearFilter;
    tex.generateMipmaps = false;
    this.#fillDataTexture(tex);
    return tex;
  }
  #fillDataTexture(tex) {
    const N = this.#samples;
    const data = tex.image.data;
    const frames = this.#curve.computeFrenetFrames(N - 1, this.#curve.closed);
    const basis = new THREE3.Matrix4();
    const quats = [];
    for (let i = 0; i < N; i++) {
      basis.makeBasis(frames.tangents[i], frames.normals[i], frames.binormals[i]);
      const q = new THREE3.Quaternion().setFromRotationMatrix(basis);
      if (i > 0 && q.dot(quats[i - 1]) < 0) {
        q.set(-q.x, -q.y, -q.z, -q.w);
      }
      quats.push(q);
    }
    const toHalf = THREE3.DataUtils.toHalfFloat;
    const writeRow = (row, i, x, y, z, w) => {
      const idx = (row * N + i) * 4;
      data[idx + 0] = toHalf(x);
      data[idx + 1] = toHalf(y);
      data[idx + 2] = toHalf(z);
      data[idx + 3] = toHalf(w);
    };
    for (let i = 0; i < N; i++) {
      const t = i / (N - 1);
      const pos = this.#curve.getPointAt(t);
      const q = quats[i];
      writeRow(0, i, pos.x, pos.y, pos.z, 0);
      writeRow(1, i, q.x, q.y, q.z, q.w);
    }
    tex.needsUpdate = true;
  }
  // Replace the spline with a new set of control points. The curve type
  // (closed) is preserved. pathSegment is re-derived from the new curve
  // length so the mesh keeps its natural span (matches setBoundsFromObject's
  // default behavior).
  setPoints(points) {
    const closed = this.#curve.closed;
    this.#curve = new THREE3.CatmullRomCurve3(points, closed);
    this.#fillDataTexture(this.#dataTexture);
    this.pathSegment.value = this.spineLength.value / Math.max(1e-6, this.#curve.getLength());
  }
  // Sets spineOffset/spineLength from a local-space bbox and updates
  // pathSegment to the ratio that preserves the mesh's original length along
  // the curve (spineLength / curveArcLength). Pass `autoSegment: false` to
  // keep the current pathSegment.
  setBoundsFromGeometryBox(box, { autoSegment = true } = {}) {
    this.spineOffset.value = -box.min.x;
    this.spineLength.value = Math.max(1e-6, box.max.x - box.min.x);
    if (autoSegment) {
      this.pathSegment.value = this.spineLength.value / Math.max(1e-6, this.#curve.getLength());
    }
  }
  setBoundsFromObject(object, options) {
    const box = new THREE3.Box3();
    object.traverse((child) => {
      if (child.isMesh && child.geometry) {
        if (!child.geometry.boundingBox) child.geometry.computeBoundingBox();
        box.union(child.geometry.boundingBox);
      }
    });
    this.setBoundsFromGeometryBox(box, options);
  }
  applyTo(target) {
    let material = target;
    if (target.isMesh) {
      target.frustumCulled = false;
      material = target.material;
    }
    const tex = this.#dataTexture;
    const pathOffsetU = this.pathOffset;
    const pathSegmentU = this.pathSegment;
    const spineOffsetU = this.spineOffset;
    const spineLengthU = this.spineLength;
    const flowU = this.flow;
    const rowCount = float3(2);
    const rowUV = (mt, row) => vec2(mt, row.add(0.5).div(rowCount));
    const rotateByQuat = (q, v) => {
      const c = cross2(q.xyz, v).add(v.mul(q.w));
      return v.add(cross2(q.xyz, c).mul(2));
    };
    material.positionNode = Fn3(() => {
      const localPos = positionLocal;
      const bend = flowU.greaterThan(0);
      const spinePortion = select2(
        bend,
        localPos.x.add(spineOffsetU).div(spineLengthU),
        float3(0)
      );
      const xWeight = select2(bend, float3(0), float3(1));
      const mt = spinePortion.mul(pathSegmentU).add(pathOffsetU);
      const spinePos = texture2(tex, rowUV(mt, float3(0))).xyz;
      const q = texture2(tex, rowUV(mt, float3(1))).normalize();
      const local = vec33(localPos.x.mul(xWeight), localPos.y, localPos.z);
      return rotateByQuat(q, local).add(spinePos);
    })();
    material.normalNode = Fn3(() => {
      const localPos = positionLocal;
      const bend = flowU.greaterThan(0);
      const spinePortion = select2(
        bend,
        localPos.x.add(spineOffsetU).div(spineLengthU),
        float3(0)
      );
      const mt = spinePortion.mul(pathSegmentU).add(pathOffsetU);
      const q = texture2(tex, rowUV(mt, float3(1))).normalize();
      return rotateByQuat(q, normalLocal).normalize();
    })();
  }
  buildHelper({ color = 65416, divisions = 200 } = {}) {
    const geo = new THREE3.BufferGeometry();
    const mat = new THREE3.LineBasicMaterial({ color, fog: false });
    const line = new THREE3.Line(geo, mat);
    line.userData.divisions = divisions;
    this.refreshHelper(line);
    return line;
  }
  // Rewrites a helper line's geometry from the current curve. Call this
  // after setPoints() to keep a previously-built helper visualization in
  // sync with the spline.
  refreshHelper(line) {
    const divisions = line.userData.divisions ?? 200;
    line.geometry.setFromPoints(this.#curve.getSpacedPoints(divisions));
  }
};

// virtual:index.js
import GUI from "lil-gui";
import Stats from "stats-gl";
var uploadedFish = window.UPLOADED_3D_MODELS?.find((m) => m.name === "clownfish.glb");
var fishUrl = uploadedFish ? uploadedFish.dataUrl : "https://omma.build/api/m/d40cd083-8e5a-447b-b824-62444e37f0f7.glb";
var params = {
  debug: false,
  fov: 30,
  toneMapping: "AgX",
  exposure: 1,
  saturation: 1.9,
  // Water/fog
  fogColor: "#06243b",
  fogNear: 1,
  fogFar: 20,
  bgColorTop: "#01040a",
  bgColorBottom: "#072a44",
  // Sun (above-water key light)
  sunColor: "#bce4ff",
  sunIntensity: 3.4,
  sunAngle: 0.55,
  sunPenumbra: 0.8,
  sunPosX: 0,
  sunPosY: 14,
  sunPosZ: 5,
  // Ambient
  ambientColor: "#0c3654",
  ambientIntensity: 0.45,
  // Hemisphere (sky-water dome)
  hemiSkyColor: "#3a9bc4",
  hemiGroundColor: "#020a14",
  hemiIntensity: 0.55,
  // Caustics (procedural noise pattern projected through the sun spotlight)
  causticsEnabled: true,
  causticColor: "#8ec8ee",
  causticStrength: 2.35,
  causticFrequency: 4,
  causticSpeed: 1.1,
  causticBaseBrightness: 0.75,
  // Sandy floor
  sandColorA: "#13334a",
  sandColorB: "#04121e",
  sandRoughness: 0.95,
  sandNoiseScale: 0.45,
  // Anemone grass (chain-of-joints simulation — bendy noodly strands, no wind)
  grassCount: 3e4,
  grassDensity: 0.1,
  grassFieldSize: 11,
  grassRadius: 5.5,
  grassBladeWidth: 1,
  grassBladeHeight: 1.8,
  grassStiffness: 0.15,
  grassDamping: 0.02,
  grassDrag: 5,
  grassBuoyancy: 0.34,
  grassGravity: 0.12,
  grassTipMass: 1.74,
  grassFlowAmplitude: 2,
  grassFlowFrequency: 0.12,
  grassFlowSpeed: 0.3,
  grassFlowPhaseOffset: 0.22,
  grassFlowVerticalAmplitude: 0,
  grassWaveAmplitude: 0,
  grassWaveSpeed: 0.3,
  grassWavePhasePerJoint: 1,
  grassSelfRight: 0.5,
  grassTiltAmplitude: 0.79,
  grassBaseColor: "#04141e",
  grassTipColor: "#4f74b0",
  grassColorVariation: 0.95,
  grassGradientFalloff: 3.19,
  grassGradientOffset: 0.33,
  grassRoughness: 0.8,
  grassMetalness: 0,
  grassClearcoat: 0,
  grassSheen: 0,
  grassIridescence: 0,
  grassIridescenceIOR: 1.3,
  grassIridescenceThicknessMin: 100,
  grassIridescenceThicknessMax: 400,
  grassTipGlow: "#ffd8e0",
  grassTipGlowIntensity: 0.2,
  grassTipGlowStart: 0.75,
  grassTipGlowFalloff: 1.35,
  grassEmissiveNoiseEnabled: true,
  grassEmissiveNoiseAmp: 0.36,
  grassEmissiveNoiseFrequency: 130.9,
  grassSpeckleEnabled: true,
  grassSpeckleAmp: 1.37,
  grassSpeckleFrequency: 290.1,
  grassSpeckleThreshold: 0.644,
  grassPushRadius: 2,
  mousePushStrength: 4,
  grassPushRecovery: 0.3,
  raycastPlaneY: 1.22,
  // Fish (deformed along a spline path)
  fishScale: 1.6,
  fishPathSpeed: 0.04,
  fishPathOffset: 0.09789880000033002,
  showPathHelper: true,
  showColliders: false,
  showJoints: false,
  // Continuous lateral sine wiggle applied across the whole curve — drives
  // the tail-waggling swim look. `wiggleFrequency` is cycles per unit arc
  // length.
  wiggleAmplitude: 0.03,
  wiggleFrequency: 1.07,
  fishColliderRadiusScale: 0.63,
  fishColliderLengthScale: 0.75,
  // Path control points (pivot-local space) — single source of truth so
  // they save through Cmd+S along with the rest of the params. Each
  // property gets its own line so the save-settings regex (line-anchored)
  // can update them without swallowing siblings.
  pathPoint0X: 0.83,
  pathPoint0Y: 0.9,
  pathPoint0Z: -2.37,
  pathPoint1X: -0.92,
  pathPoint1Y: 0.9,
  pathPoint1Z: 0,
  pathPoint2X: 1.41,
  pathPoint2Y: 0.9,
  pathPoint2Z: 1.7,
  pathPoint3X: -1.5,
  pathPoint3Y: 0.9,
  pathPoint3Z: 2.58,
  pathPoint4X: -2.95,
  pathPoint4Y: 0.9,
  pathPoint4Z: 0.54,
  pathPoint5X: -1.79,
  pathPoint5Y: 0.9,
  pathPoint5Z: -2.35,
  // Particles (floating plankton — camera-facing sprites with a procedural
  // silhouette generated in the fragment shader).
  showPlankton: true,
  particleCount: 3e3,
  particleSizeMin: 0.01,
  particleSizeMax: 0.015,
  particleSpeed: 0.3,
  particleColor: "#cfe9ff",
  particleOpacity: 0.8,
  particleStretchMin: 1,
  particleStretchMax: 3,
  particleGranule: 1,
  particleEdge: 0.125,
  particleLighting: 1,
  particleVolumeXZ: 16,
  particleVolumeY: 6.5,
  particleVolumeYBase: 0.2,
  // Corals — per-coral XYZ position and scale.
  yellowOrangeCoralX: -2.35,
  yellowOrangeCoralY: 0,
  yellowOrangeCoralZ: -3.17,
  yellowOrangeCoralScale: 5.2,
  yellowOrangeCoralColliderRadius: 0.43,
  redCoralX: 2.8,
  redCoralY: 1.01,
  redCoralZ: -1.65,
  redCoralScale: 1.87,
  redCoralColliderRadius: 0.43,
  vibrantAcroporaX: -2.7,
  vibrantAcroporaY: 0.35,
  vibrantAcroporaZ: 2.28,
  vibrantAcroporaScale: 2.41,
  vibrantAcroporaColliderRadius: 0.5,
  brainCoralX: 3.35,
  brainCoralY: 0.2,
  brainCoralZ: 0.09,
  brainCoralScale: 2.41,
  brainCoralColliderRadius: 0.6,
  showCorals: true
};
var toneMappingOptions = {
  None: THREE4.NoToneMapping,
  Linear: THREE4.LinearToneMapping,
  Reinhard: THREE4.ReinhardToneMapping,
  Cineon: THREE4.CineonToneMapping,
  ACESFilmic: THREE4.ACESFilmicToneMapping,
  AgX: THREE4.AgXToneMapping,
  Neutral: THREE4.NeutralToneMapping
};
var renderer = new THREE4.WebGPURenderer({
  antialias: true,
  requiredLimits: { maxStorageBuffersInVertexStage: 2 }
});
renderer.setPixelRatio(Math.min(devicePixelRatio, 1));
renderer.setSize(innerWidth, innerHeight);
renderer.toneMapping = toneMappingOptions[params.toneMapping];
renderer.toneMappingExposure = params.exposure;
renderer.outputColorSpace = THREE4.SRGBColorSpace;
renderer.shadowMap.enabled = true;
renderer.shadowMap.type = THREE4.PCFSoftShadowMap;
(document.getElementById("root") ?? document.body).appendChild(renderer.domElement);
await renderer.init();
var stats = new Stats({ trackGPU: true, trackCPT: true });
(document.getElementById("root") ?? document.body).appendChild(stats.dom);
stats.init(renderer);
var scene = new THREE4.Scene();
scene.fog = new THREE4.Fog(params.fogColor, params.fogNear, params.fogFar);
var camera = new THREE4.PerspectiveCamera(params.fov, innerWidth / innerHeight, 0.1, 100);
camera.position.set(0, 4.38, 8.62);
var controls = new OrbitControls(camera, renderer.domElement);
controls.enableDamping = true;
controls.dampingFactor = 0.06;
controls.target.set(0, 0.52, -0.04);
controls.minDistance = 5.5;
controls.maxDistance = 15;
controls.maxPolarAngle = Math.PI * 0.48;
var saturationU = uniform4(params.saturation);
var postProcessing = new THREE4.PostProcessing(renderer);
var scenePass = pass(scene, camera);
postProcessing.outputNode = saturation(scenePass, saturationU);
var bgCanvas = document.createElement("canvas");
bgCanvas.width = 2;
bgCanvas.height = 512;
var bgCtx = bgCanvas.getContext("2d");
var bgTexture = new THREE4.CanvasTexture(bgCanvas);
bgTexture.colorSpace = THREE4.SRGBColorSpace;
function updateGradientBackground() {
  const grad = bgCtx.createLinearGradient(0, 0, 0, bgCanvas.height);
  grad.addColorStop(0, params.bgColorBottom);
  grad.addColorStop(1, params.bgColorTop);
  bgCtx.fillStyle = grad;
  bgCtx.fillRect(0, 0, bgCanvas.width, bgCanvas.height);
  bgTexture.needsUpdate = true;
}
updateGradientBackground();
var bgMaterial = new THREE4.MeshBasicNodeMaterial({ map: bgTexture, fog: false, depthWrite: false });
var bgMesh = new THREE4.Mesh(new THREE4.PlaneGeometry(1, 1), bgMaterial);
bgMesh.frustumCulled = false;
bgMesh.renderOrder = -1;
camera.add(bgMesh);
scene.add(camera);
function updateBackgroundPlane() {
  const depth = 22;
  bgMesh.position.z = -depth;
  const h = 2 * depth * Math.tan(camera.fov * Math.PI / 360);
  const w = h * camera.aspect;
  bgMesh.scale.set(w, h, 1);
}
updateBackgroundPlane();
var ambient = new THREE4.AmbientLight(params.ambientColor, params.ambientIntensity);
scene.add(ambient);
var hemi = new THREE4.HemisphereLight(params.hemiSkyColor, params.hemiGroundColor, params.hemiIntensity);
scene.add(hemi);
var sun = new THREE4.SpotLight(params.sunColor, params.sunIntensity, 0, params.sunAngle, params.sunPenumbra, 0);
sun.position.set(params.sunPosX, params.sunPosY, params.sunPosZ);
sun.target.position.set(0, 0, 0);
sun.castShadow = true;
sun.shadow.mapSize.setScalar(1024);
sun.shadow.camera.near = 1;
sun.shadow.camera.far = 30;
sun.shadow.bias = -5e-4;
sun.shadow.normalBias = 0.04;
sun.shadow.radius = 4;
scene.add(sun);
scene.add(sun.target);
var sunHelper = new THREE4.SpotLightHelper(sun);
sunHelper.visible = false;
scene.add(sunHelper);
var proceduralCaustics = new ProceduralCaustics(renderer, {
  color: params.causticColor,
  strength: params.causticStrength,
  frequency: params.causticFrequency,
  speed: params.causticSpeed,
  baseBrightness: params.causticBaseBrightness
});
if (params.causticsEnabled) sun.map = proceduralCaustics.texture;
var sandColorAU = uniform4(new THREE4.Color(params.sandColorA));
var sandColorBU = uniform4(new THREE4.Color(params.sandColorB));
var sandNoiseScaleU = uniform4(params.sandNoiseScale);
var sandBaseColor = Fn4(() => {
  const p = positionWorld.xz.mul(sandNoiseScaleU);
  const n1 = mx_noise_float3(vec34(p.x, float4(0), p.y)).mul(0.5).add(0.5);
  const n2 = mx_noise_float3(vec34(p.x.mul(3.2), float4(7), p.y.mul(3.2))).mul(0.5).add(0.5);
  const n = n1.mul(0.65).add(n2.mul(0.35));
  return mix2(sandColorBU, sandColorAU, n);
})();
var sandMat = new THREE4.MeshStandardNodeMaterial({
  roughness: params.sandRoughness,
  metalness: 0
});
sandMat.colorNode = sandBaseColor;
var sandFloor = new THREE4.Mesh(new THREE4.CircleGeometry(40, 96), sandMat);
sandFloor.rotation.x = -Math.PI / 2;
sandFloor.receiveShadow = true;
scene.add(sandFloor);
var jointsPerBlade = 8;
var bladeRadius = 0.05;
var bladeCapsuleHeight = 1 - 2 * bladeRadius;
var tubeGeo = new THREE4.CapsuleGeometry(bladeRadius, bladeCapsuleHeight, 3, 6, jointsPerBlade + 1);
tubeGeo.translate(0, 0.5, 0);
var tipMaskCanvas = document.createElement("canvas");
tipMaskCanvas.width = 4;
tipMaskCanvas.height = 256;
var tipMaskCtx = tipMaskCanvas.getContext("2d");
function paintTipMask() {
  const start = params.grassTipGlowStart;
  const falloff = params.grassTipGlowFalloff;
  const w = tipMaskCanvas.width;
  const h = tipMaskCanvas.height;
  const image = tipMaskCtx.createImageData(w, h);
  for (let y = 0; y < h; y++) {
    const t = 1 - y / (h - 1);
    let mask = 0;
    if (t > start) {
      const u = (t - start) / (1 - start);
      mask = u ** falloff;
    }
    const v = Math.round(Math.min(Math.max(mask, 0), 1) * 255);
    for (let x = 0; x < w; x++) {
      const i = (y * w + x) * 4;
      image.data[i] = v;
      image.data[i + 1] = v;
      image.data[i + 2] = v;
      image.data[i + 3] = 255;
    }
  }
  tipMaskCtx.putImageData(image, 0, 0);
}
paintTipMask();
var tipMaskTex = new THREE4.CanvasTexture(tipMaskCanvas);
tipMaskTex.colorSpace = THREE4.NoColorSpace;
tipMaskTex.minFilter = THREE4.LinearFilter;
tipMaskTex.magFilter = THREE4.LinearFilter;
var grassMaterial = new THREE4.MeshPhysicalNodeMaterial({
  side: THREE4.FrontSide,
  roughness: params.grassRoughness,
  metalness: params.grassMetalness,
  clearcoat: params.grassClearcoat,
  sheen: params.grassSheen,
  iridescence: params.grassIridescence,
  iridescenceIOR: params.grassIridescenceIOR,
  iridescenceThicknessRange: [params.grassIridescenceThicknessMin, params.grassIridescenceThicknessMax],
  emissive: new THREE4.Color(params.grassTipGlow),
  emissiveIntensity: params.grassTipGlowIntensity,
  emissiveMap: tipMaskTex
});
var grass = new GrassNoodles(renderer, {
  bladeCount: params.grassCount,
  jointsPerBlade,
  density: params.grassDensity,
  gridFieldSize: params.grassFieldSize,
  bladeWidth: params.grassBladeWidth,
  bladeHeight: params.grassBladeHeight,
  stiffness: params.grassStiffness,
  damping: params.grassDamping,
  drag: params.grassDrag,
  buoyancy: params.grassBuoyancy,
  gravity: params.grassGravity,
  tipMass: params.grassTipMass,
  flowAmplitude: params.grassFlowAmplitude,
  flowFrequency: params.grassFlowFrequency,
  flowSpeed: params.grassFlowSpeed,
  flowPhaseOffset: params.grassFlowPhaseOffset,
  flowVerticalAmplitude: params.grassFlowVerticalAmplitude,
  waveAmplitude: params.grassWaveAmplitude,
  waveSpeed: params.grassWaveSpeed,
  wavePhasePerJoint: params.grassWavePhasePerJoint,
  selfRight: params.grassSelfRight,
  tiltAmplitude: params.grassTiltAmplitude,
  bladeBaseColor: params.grassBaseColor,
  bladeTipColor: params.grassTipColor,
  bladeColorVariation: params.grassColorVariation,
  bladeGradientFalloff: params.grassGradientFalloff,
  bladeGradientOffset: params.grassGradientOffset,
  bladeGeometry: tubeGeo,
  material: grassMaterial,
  pushRadius: params.grassPushRadius,
  pushStrengths: [params.mousePushStrength],
  colliderTypes: ["push", "collision"],
  pushRecovery: params.grassPushRecovery,
  positioningNode: (idx) => {
    const cols = 175;
    const col = idx.mod(cols);
    const row = idx.div(cols);
    const jx = hash2(idx).sub(0.5);
    const jz = hash2(idx.add(7919)).sub(0.5);
    const wx = col.toFloat().add(jx).div(float4(cols)).sub(0.5).mul(params.grassFieldSize);
    const wz = row.toFloat().add(jz).div(float4(cols)).sub(0.5).mul(params.grassFieldSize);
    const dist = sqrt2(wx.mul(wx).add(wz.mul(wz)));
    const edgeNoise = mx_noise_float3(vec34(wx.mul(0.22).add(11), float4(0), wz.mul(0.22).add(11))).mul(0.5).add(0.5);
    const maxR = float4(params.grassRadius).add(edgeNoise.sub(0.5).mul(float4(params.grassRadius).mul(0.35)));
    const isShown = float4(1).sub(smoothstep3(maxR.sub(float4(params.grassRadius).mul(0.2)), maxR, dist));
    return { wx, wz, isShown };
  }
});
grass.init();
grass.mesh.castShadow = true;
grass.mesh.receiveShadow = true;
scene.add(grass.group);
var totalJointVerts = params.grassCount * jointsPerBlade;
var wireSegmentsPerBlade = jointsPerBlade - 1;
var wireIndices = new Uint32Array(params.grassCount * wireSegmentsPerBlade * 2);
for (let b = 0, w = 0; b < params.grassCount; b++) {
  const base = b * jointsPerBlade;
  for (let s = 0; s < wireSegmentsPerBlade; s++) {
    wireIndices[w++] = base + s;
    wireIndices[w++] = base + s + 1;
  }
}
var jointsWireGeo = new THREE4.BufferGeometry();
jointsWireGeo.setAttribute("position", new THREE4.Float32BufferAttribute(new Float32Array(totalJointVerts * 3), 3));
jointsWireGeo.setIndex(new THREE4.BufferAttribute(wireIndices, 1));
var jointsWireMat = new THREE4.LineBasicMaterial({ color: 16738047, fog: false });
jointsWireMat.positionNode = Fn4(() => {
  const bladeIdx = vertexIndex.div(jointsPerBlade);
  const shown = grass.bladeIsShown.element(bladeIdx);
  return grass.positions.element(vertexIndex).xyz.mul(shown);
})();
var jointsWire = new THREE4.LineSegments(jointsWireGeo, jointsWireMat);
jointsWire.frustumCulled = false;
jointsWire.visible = params.debug && params.showJoints;
scene.add(jointsWire);
var grassEmissiveColorU = uniform4(new THREE4.Color(params.grassTipGlow));
var grassEmissiveIntensityU = uniform4(params.grassTipGlowIntensity);
var grassEmissiveNoiseEnabledU = uniform4(params.grassEmissiveNoiseEnabled ? 1 : 0);
var grassEmissiveNoiseAmpU = uniform4(params.grassEmissiveNoiseAmp);
var grassEmissiveNoiseFreqU = uniform4(params.grassEmissiveNoiseFrequency);
var grassSpeckleEnabledU = uniform4(params.grassSpeckleEnabled ? 1 : 0);
var grassSpeckleAmpU = uniform4(params.grassSpeckleAmp);
var grassSpeckleFreqU = uniform4(params.grassSpeckleFrequency);
var grassSpeckleThresholdU = uniform4(params.grassSpeckleThreshold);
grassMaterial.emissiveNode = Fn4(() => {
  const mask = texture3(tipMaskTex).r;
  const baseGlow = grassEmissiveColorU.mul(grassEmissiveIntensityU).mul(mask);
  const noiseMod = float4(1).toVar();
  If2(grassEmissiveNoiseEnabledU.equal(float4(1)), () => {
    const instOffset = hash2(instanceIndex2).mul(100);
    const np = positionGeometry2.mul(grassEmissiveNoiseFreqU).add(instOffset);
    const n = mx_noise_float3(np).mul(0.5).add(0.5);
    noiseMod.assign(mix2(float4(1), n, grassEmissiveNoiseAmpU));
  });
  const baseEmissive = baseGlow.mul(noiseMod);
  const speckleEmissive = vec34(0).toVar();
  If2(grassSpeckleEnabledU.equal(float4(1)), () => {
    const spOffset = hash2(instanceIndex2.add(31)).mul(100);
    const sp = positionGeometry2.mul(grassSpeckleFreqU).add(spOffset);
    const spNoise = mx_noise_float3(sp).mul(0.5).add(0.5);
    const speckleMask = smoothstep3(grassSpeckleThresholdU, float4(1), spNoise);
    speckleEmissive.assign(grassEmissiveColorU.mul(grassEmissiveIntensityU).mul(speckleMask).mul(grassSpeckleAmpU));
  });
  return baseEmissive.add(speckleEmissive);
})();
var particleSizeMinU = uniform4(params.particleSizeMin);
var particleSizeMaxU = uniform4(params.particleSizeMax);
var particleSpeedU = uniform4(params.particleSpeed);
var particleColorU = uniform4(new THREE4.Color(params.particleColor));
var particleOpacityU = uniform4(params.particleOpacity);
var particleStretchMinU = uniform4(params.particleStretchMin);
var particleStretchMaxU = uniform4(params.particleStretchMax);
var particleGranuleU = uniform4(params.particleGranule);
var particleEdgeU = uniform4(params.particleEdge);
var particleLightingU = uniform4(params.particleLighting);
var particleVolumeXZU = uniform4(params.particleVolumeXZ);
var particleVolumeYU = uniform4(params.particleVolumeY);
var particleVolumeYBaseU = uniform4(params.particleVolumeYBase);
var pSeedX = hash2(instanceIndex2.add(101));
var pSeedY = hash2(instanceIndex2.add(202));
var pSeedZ = hash2(instanceIndex2.add(303));
var pSeedR = hash2(instanceIndex2.add(404));
var pSeedS = hash2(instanceIndex2.add(505));
var pSeedH = hash2(instanceIndex2.add(606));
var pSeedT = hash2(instanceIndex2.add(707));
var particleMaterial = new THREE4.SpriteNodeMaterial({
  transparent: true,
  depthWrite: false,
  blending: THREE4.AdditiveBlending
});
particleMaterial.positionNode = Fn4(() => {
  const t = time3.mul(particleSpeedU);
  const baseX = pSeedX.sub(0.5).mul(particleVolumeXZU);
  const baseZ = pSeedZ.sub(0.5).mul(particleVolumeXZU);
  const swayX = sin2(t.mul(0.7).add(pSeedX.mul(10))).mul(0.6);
  const swayZ = cos2(t.mul(0.5).add(pSeedZ.mul(10))).mul(0.6);
  const fallY = fract2(pSeedY.add(t.mul(0.04))).mul(particleVolumeYU).add(particleVolumeYBaseU);
  return vec34(baseX.add(swayX), fallY, baseZ.add(swayZ));
})();
particleMaterial.scaleNode = Fn4(() => {
  const w = mix2(particleSizeMinU, particleSizeMaxU, pSeedS);
  const stretch = mix2(particleStretchMinU, particleStretchMaxU, pSeedT);
  return vec22(w, w.mul(stretch));
})();
var pRotation = pSeedR.mul(Math.PI * 2);
particleMaterial.rotationNode = pRotation;
particleMaterial.opacityNode = Fn4(() => {
  const p = uv3().sub(0.5);
  const r = p.length();
  const haloInner = float4(0.5).sub(particleEdgeU);
  const halo = smoothstep3(float4(0.5), haloInner, r).mul(0.5);
  const core = smoothstep3(float4(0.2), float4(0), r).mul(0.7);
  const body = halo.add(core);
  const granule = mx_noise_float3(vec34(uv3().mul(7), float4(0))).mul(0.5).add(0.5);
  const speckled = mix2(float4(1).sub(particleGranuleU), float4(1), granule);
  return body.mul(speckled).mul(particleOpacityU);
})();
particleMaterial.colorNode = Fn4(() => {
  const hueShift = pSeedH.sub(0.5).mul(0.3);
  const base = particleColorU.add(vec34(hueShift.negate(), hueShift.mul(0.5), float4(0)));
  const viewUpXY = cameraViewMatrix.mul(vec43(0, 1, 0, 0)).xy.normalize();
  const upInUV = rotate(viewUpXY, pRotation.negate());
  const topness = uv3().sub(0.5).dot(upInUV).mul(2).add(0.5).clamp(0, 1);
  const lightFactor = mix2(float4(1).sub(particleLightingU), float4(1), topness);
  return base.mul(lightFactor);
})();
var particles = new THREE4.Sprite(particleMaterial);
particles.count = params.particleCount;
particles.frustumCulled = false;
particles.visible = params.showPlankton;
scene.add(particles);
var dracoLoader = new DRACOLoader();
dracoLoader.setDecoderPath("https://www.gstatic.com/draco/versioned/decoders/1.5.7/");
var gltfLoader = new GLTFLoader();
gltfLoader.setDRACOLoader(dracoLoader);
var fishGroup = new THREE4.Group();
fishGroup.name = "fishGroup";
scene.add(fishGroup);
var fishPivot = null;
var pathFollow = null;
var pathHelper = null;
var pathPointsHelper = null;
var pathSampleCount = 300;
var pathPointCount = 6;
function getPathControlPoints() {
  const pts = [];
  for (let i = 0; i < pathPointCount; i++) {
    pts.push(new THREE4.Vector3(params[`pathPoint${i}X`], params[`pathPoint${i}Y`], params[`pathPoint${i}Z`]));
  }
  return pts;
}
function buildWigglePath() {
  const stops = getPathControlPoints();
  const closed = true;
  const guide = new THREE4.CatmullRomCurve3(stops, closed);
  const totalLength = guide.getLength();
  const N = pathSampleCount;
  const samples = [];
  const tangents = [];
  for (let i = 0; i < N; i++) {
    const u = i / N;
    samples.push(guide.getPointAt(u));
    tangents.push(guide.getTangentAt(u));
  }
  const amplitude = params.wiggleAmplitude;
  const frequency = params.wiggleFrequency;
  if (amplitude === 0 || frequency === 0) return samples;
  const perp = new THREE4.Vector3();
  for (let i = 0; i < N; i++) {
    const arcPos = i / N * totalLength;
    const offset = Math.sin(arcPos * 2 * Math.PI * frequency) * amplitude;
    const tan = tangents[i];
    perp.set(-tan.z, 0, tan.x).normalize();
    samples[i].addScaledVector(perp, offset);
  }
  return samples;
}
async function loadFish(url) {
  const gltf = await gltfLoader.loadAsync(url);
  const model = gltf.scene;
  model.name = "clownfish";
  const box = new THREE4.Box3().setFromObject(model);
  const size = box.getSize(new THREE4.Vector3());
  const maxDim = Math.max(size.x, size.y, size.z);
  if (maxDim > 0) model.scale.multiplyScalar(1 / maxDim);
  model.updateMatrixWorld(true);
  const orientedSize = new THREE4.Box3().setFromObject(model).getSize(new THREE4.Vector3());
  const axes = ["x", "y", "z"];
  const longestAxis = axes.reduce((a, b) => orientedSize[b] > orientedSize[a] ? b : a);
  if (longestAxis === "z") model.rotation.y = Math.PI / 2;
  else if (longestAxis === "y") model.rotation.z = Math.PI / 2;
  model.quaternion.premultiply(new THREE4.Quaternion().setFromAxisAngle(new THREE4.Vector3(1, 0, 0), Math.PI));
  model.updateMatrixWorld(true);
  const centerBox = new THREE4.Box3().setFromObject(model);
  const center = centerBox.getCenter(new THREE4.Vector3());
  model.position.sub(center);
  model.updateMatrixWorld(true);
  const meshes = [];
  model.traverse((child) => {
    if (child.isMesh && child.geometry) {
      child.updateMatrixWorld(true);
      child.geometry.applyMatrix4(child.matrixWorld);
      child.geometry.computeBoundingBox();
      child.geometry.computeVertexNormals();
      child.castShadow = true;
      child.receiveShadow = true;
      meshes.push(child);
    }
  });
  const fishMesh = new THREE4.Group();
  fishMesh.name = "clownfish";
  for (const mesh of meshes) {
    mesh.parent?.remove(mesh);
    mesh.position.set(0, 0, 0);
    mesh.rotation.set(0, 0, 0);
    mesh.scale.set(1, 1, 1);
    mesh.updateMatrix();
    fishMesh.add(mesh);
  }
  const pivot = new THREE4.Group();
  pivot.name = "fishPivot";
  pivot.add(fishMesh);
  fishGroup.add(pivot);
  fishPivot = pivot;
  pathFollow = new PathFollow({
    points: buildWigglePath(),
    samples: 256,
    closed: true,
    flow: true
  });
  pathFollow.pathOffset.value = params.fishPathOffset;
  pathFollow.setBoundsFromObject(fishMesh);
  for (const mesh of meshes) pathFollow.applyTo(mesh);
  pathHelper = pathFollow.buildHelper({ color: 65416, divisions: 256 });
  pathHelper.visible = params.debug && params.showPathHelper;
  pivot.add(pathHelper);
  pathPointsHelper = new THREE4.Group();
  pathPointsHelper.visible = params.debug && params.showPathHelper;
  pivot.add(pathPointsHelper);
  rebuildPathPointsHelper();
  const colliderBox = new THREE4.Box3();
  for (const mesh of meshes) {
    if (mesh.geometry?.boundingBox) colliderBox.union(mesh.geometry.boundingBox);
  }
  const colliderSize = colliderBox.getSize(new THREE4.Vector3());
  const colliderRadius = Math.max(colliderSize.y, colliderSize.z) / 2 * 1.1;
  const colliderLength = Math.max(1e-3, colliderSize.x - 2 * colliderRadius) * 1.95;
  const colliderGeo = new THREE4.CapsuleGeometry(colliderRadius, colliderLength, 4, 12);
  const colliderMat = new THREE4.MeshBasicMaterial({
    color: 65416,
    wireframe: true,
    fog: false,
    transparent: true,
    opacity: 0.7
  });
  const collider = new THREE4.Mesh(colliderGeo, colliderMat);
  collider.visible = params.debug && params.showColliders;
  collider.userData.colliderRadius = colliderRadius;
  collider.userData.colliderLength = colliderLength;
  collider.userData.baseColliderRadius = colliderRadius;
  collider.userData.baseColliderLength = colliderLength;
  scene.add(collider);
  pivot.userData.collider = collider;
  return pivot;
}
function rebuildFishCollider() {
  if (!fishPivot?.userData.collider) return;
  const collider = fishPivot.userData.collider;
  const baseRadius = collider.userData.baseColliderRadius;
  const baseLength = collider.userData.baseColliderLength;
  const newRadius = baseRadius * params.fishColliderRadiusScale;
  const newLength = baseLength * params.fishColliderLengthScale;
  collider.userData.colliderRadius = newRadius;
  collider.userData.colliderLength = newLength;
  const newGeo = new THREE4.CapsuleGeometry(newRadius, newLength, 4, 12);
  collider.geometry.dispose();
  collider.geometry = newGeo;
}
function applyFishTransform() {
  if (!fishPivot) return;
  fishPivot.scale.setScalar(params.fishScale);
}
var coralDefs = [
  {
    name: "Red Coral",
    id: "redCoral",
    url: "https://omma.build/api/m/89a1d0f2-8ed9-474b-af94-ca372d50c46d.glb"
  },
  {
    name: "Vibrant Acropora",
    id: "vibrantAcropora",
    url: "https://omma.build/api/m/78d505e8-b5a7-4713-9a37-4af389dcb5c4.glb"
  },
  {
    name: "Yellow/Orange Coral",
    id: "yellowOrangeCoral",
    url: "https://omma.build/api/m/ba7f0725-6f08-499d-a1a1-e602ca9ff2a9.glb"
  },
  {
    name: "Brain Coral",
    id: "brainCoral",
    url: "https://omma.build/api/m/f7097382-0650-4d99-9e81-575c74f9c5d6.glb"
  }
];
var coralsGroup = new THREE4.Group();
coralsGroup.name = "corals";
scene.add(coralsGroup);
var coralColliderMat = new THREE4.MeshBasicMaterial({
  color: 16737962,
  wireframe: true,
  fog: false,
  transparent: true,
  opacity: 0.7
});
var coralInstances = [];
var CORAL_COLLIDER_SLOT_START = 2;
var coralWorldPos = new THREE4.Vector3();
function applyCoralTransform(inst) {
  inst.pivot.position.set(params[`${inst.id}X`], params[`${inst.id}Y`], params[`${inst.id}Z`]);
  const scale = params[`${inst.id}Scale`];
  inst.pivot.scale.setScalar(scale);
  const radius = params[`${inst.id}ColliderRadius`];
  inst.collider.scale.setScalar(radius);
  inst.pivot.updateMatrixWorld(true);
  coralWorldPos.copy(inst.localSphereCenter).applyMatrix4(inst.pivot.matrixWorld);
  const slot = grass.colliders[inst.slotIdx];
  slot.position.copy(coralWorldPos);
  slot.endpoint.copy(coralWorldPos);
  slot.radius = radius * scale;
}
async function loadCoral(def, slotIdx) {
  const pivot = new THREE4.Group();
  pivot.name = `${def.id}Pivot`;
  const localSphereCenter = new THREE4.Vector3(0, 0.5, 0);
  const collider = new THREE4.Mesh(new THREE4.SphereGeometry(1, 16, 12), coralColliderMat);
  collider.position.copy(localSphereCenter);
  collider.visible = params.debug && params.showColliders;
  pivot.add(collider);
  coralsGroup.add(pivot);
  grass.colliders[slotIdx].type = "collision";
  const inst = {
    id: def.id,
    name: def.name,
    pivot,
    collider,
    slotIdx,
    localSphereCenter
  };
  coralInstances.push(inst);
  applyCoralTransform(inst);
  try {
    const gltf = await gltfLoader.loadAsync(def.url);
    const model = gltf.scene;
    model.name = def.id;
    model.traverse((obj) => {
      if (obj.isMesh) {
        obj.castShadow = true;
        obj.receiveShadow = true;
      }
    });
    pivot.add(model);
    pivot.updateMatrixWorld(true);
    const localBox = new THREE4.Box3().setFromObject(model);
    const localSphere = localBox.getBoundingSphere(new THREE4.Sphere());
    const invPivot = new THREE4.Matrix4().copy(pivot.matrixWorld).invert();
    localSphere.center.applyMatrix4(invPivot);
    inst.localSphereCenter.copy(localSphere.center);
    collider.position.copy(localSphere.center);
    applyCoralTransform(inst);
  } catch (err) {
    console.warn(`Failed to load coral model ${def.id}:`, err);
  }
}
var loaderEl = document.getElementById("loader");
var coralPromises = coralDefs.map((def, i) => loadCoral(def, CORAL_COLLIDER_SLOT_START + i));
var fishPromise = loadFish(fishUrl).then(() => {
  applyFishTransform();
  rebuildFishCollider();
});
Promise.allSettled([fishPromise, ...coralPromises]).then(() => {
  loaderEl.classList.add("hidden");
});
var gui = new GUI({ closeFolders: true });
var uiContainer = document.querySelector(".ui-container");
function setDebug(v) {
  params.debug = v;
  stats.dom.style.display = v ? "" : "none";
  gui.domElement.style.display = v ? "" : "none";
  if (uiContainer) uiContainer.style.opacity = v ? "0" : "1";
  sunHelper.visible = v;
  if (fishPivot?.userData.collider) fishPivot.userData.collider.visible = v && params.showColliders;
  if (pathHelper) pathHelper.visible = v && params.showPathHelper;
  if (pathPointsHelper) pathPointsHelper.visible = v && params.showPathHelper;
  for (const inst of coralInstances) inst.collider.visible = v && params.showColliders;
  jointsWire.visible = v && params.showJoints;
  grass.mesh.visible = !(v && params.showJoints);
}
gui.add(params, "debug").name("Debug (P)").onChange(setDebug);
gui.add(params, "showColliders").name("Show Colliders").onChange((v) => {
  if (fishPivot?.userData.collider) fishPivot.userData.collider.visible = params.debug && v;
  for (const inst of coralInstances) inst.collider.visible = params.debug && v;
});
addEventListener("keydown", (e) => {
  if (e.key === "p" || e.key === "P") {
    setDebug(!params.debug);
    gui.controllers[0].updateDisplay();
  }
});
setDebug(params.debug);
gui.add(params, "fov", 10, 90, 1).name("FOV").onChange((v) => {
  camera.fov = v;
  camera.updateProjectionMatrix();
  updateBackgroundPlane();
});
gui.add(
  {
    logCamera: () => {
      const p = camera.position;
      const t = controls.target;
      console.log(`Camera position: (${p.x.toFixed(3)}, ${p.y.toFixed(3)}, ${p.z.toFixed(3)})`);
      console.log(`Camera target:   (${t.x.toFixed(3)}, ${t.y.toFixed(3)}, ${t.z.toFixed(3)})`);
    }
  },
  "logCamera"
).name("Log Camera");
gui.add(params, "toneMapping", Object.keys(toneMappingOptions)).name("Tone Mapping").onChange((v) => renderer.toneMapping = toneMappingOptions[v]);
gui.add(params, "exposure", 0, 3, 0.01).name("Exposure").onChange((v) => renderer.toneMappingExposure = v);
gui.add(params, "saturation", 0, 3, 0.01).name("Saturation").onChange((v) => saturationU.value = v);
var fogFolder = gui.addFolder("Water / Fog");
fogFolder.addColor(params, "fogColor").name("Fog Color").onChange((v) => scene.fog.color.set(v));
fogFolder.add(params, "fogNear", 0, 30, 0.1).name("Fog Near").onChange((v) => scene.fog.near = v);
fogFolder.add(params, "fogFar", 0, 60, 0.1).name("Fog Far").onChange((v) => scene.fog.far = v);
fogFolder.addColor(params, "bgColorTop").name("BG Top").onChange(updateGradientBackground);
fogFolder.addColor(params, "bgColorBottom").name("BG Bottom").onChange(updateGradientBackground);
var lightFolder = gui.addFolder("Lights");
lightFolder.addColor(params, "sunColor").name("Sun Color").onChange((v) => sun.color.set(v));
lightFolder.add(params, "sunIntensity", 0, 10, 0.05).name("Sun Intensity").onChange((v) => sun.intensity = v);
lightFolder.add(params, "sunAngle", 0.05, Math.PI / 2, 0.01).name("Sun Angle").onChange((v) => sun.angle = v);
lightFolder.add(params, "sunPenumbra", 0, 1, 0.01).name("Sun Penumbra").onChange((v) => sun.penumbra = v);
lightFolder.add(params, "sunPosX", -20, 20, 0.1).name("Sun X").onChange((v) => sun.position.setX(v));
lightFolder.add(params, "sunPosY", 0, 30, 0.1).name("Sun Y").onChange((v) => sun.position.setY(v));
lightFolder.add(params, "sunPosZ", -20, 20, 0.1).name("Sun Z").onChange((v) => sun.position.setZ(v));
lightFolder.addColor(params, "ambientColor").name("Ambient").onChange((v) => ambient.color.set(v));
lightFolder.add(params, "ambientIntensity", 0, 3, 0.01).name("Ambient Intensity").onChange((v) => ambient.intensity = v);
lightFolder.addColor(params, "hemiSkyColor").name("Hemi Sky").onChange((v) => hemi.color.set(v));
lightFolder.addColor(params, "hemiGroundColor").name("Hemi Ground").onChange((v) => hemi.groundColor.set(v));
lightFolder.add(params, "hemiIntensity", 0, 3, 0.01).name("Hemi Intensity").onChange((v) => hemi.intensity = v);
var causticFolder = gui.addFolder("Caustics");
causticFolder.add(params, "causticsEnabled").name("Enabled").onChange((v) => sun.map = v ? proceduralCaustics.texture : null);
causticFolder.addColor(params, "causticColor").name("Color").onChange((v) => proceduralCaustics.uniforms.color.value.set(v));
causticFolder.add(params, "causticStrength", 0, 6, 0.01).name("Strength").onChange((v) => proceduralCaustics.uniforms.strength.value = v);
causticFolder.add(params, "causticFrequency", 0.5, 20, 0.1).name("Frequency").onChange((v) => proceduralCaustics.uniforms.frequency.value = v);
causticFolder.add(params, "causticSpeed", 0, 3, 0.01).name("Speed").onChange((v) => proceduralCaustics.uniforms.speed.value = v);
causticFolder.add(params, "causticBaseBrightness", 0, 2, 0.01).name("Base Brightness").onChange((v) => proceduralCaustics.uniforms.baseBrightness.value = v);
var sandFolder = gui.addFolder("Sand");
sandFolder.addColor(params, "sandColorA").name("Sand A").onChange((v) => sandColorAU.value.set(v));
sandFolder.addColor(params, "sandColorB").name("Sand B").onChange((v) => sandColorBU.value.set(v));
sandFolder.add(params, "sandNoiseScale", 0.05, 3, 0.01).name("Noise Scale").onChange((v) => sandNoiseScaleU.value = v);
var simFolder = gui.addFolder("Anemone Simulation");
simFolder.add(params, "showJoints").name("Show Joints").onChange((v) => {
  const on = params.debug && v;
  jointsWire.visible = on;
  grass.mesh.visible = !on;
});
simFolder.add(params, "grassFieldSize", 2, 30, 0.5).name("Field Size").onChange((v) => {
  grass.gridFieldSize = v;
  params.grassRadius = v * 0.5;
  gui.controllersRecursive().find((c) => c.property === "grassRadius")?.updateDisplay();
  renderer.compute(grass.computeInit);
});
simFolder.add(params, "grassRadius", 0.5, 15, 0.1).name("Radius").onChange(() => {
  renderer.compute(grass.computeInit);
});
simFolder.add(params, "grassDensity", 0, 1, 0.01).name("Density").onChange((v) => {
  grass.density = v;
  renderer.compute(grass.computeInit);
});
simFolder.add(params, "grassStiffness", 0, 1, 0.01).name("Stiffness").onChange((v) => grass.stiffness = v);
simFolder.add(params, "grassDamping", 0, 0.5, 5e-3).name("Damping").onChange((v) => grass.damping = v);
simFolder.add(params, "grassDrag", 0, 30, 0.1).name("Drag").onChange((v) => grass.drag = v);
simFolder.add(params, "grassBuoyancy", 0, 0.5, 5e-3).name("Buoyancy").onChange((v) => grass.buoyancy = v);
simFolder.add(params, "grassGravity", 0, 2, 0.01).name("Gravity").onChange((v) => grass.gravity = v);
simFolder.add(params, "grassTipMass", 0, 3, 0.01).name("Tip Mass").onChange((v) => grass.tipMass = v);
simFolder.add(params, "grassFlowAmplitude", 0, 8, 0.05).name("Flow Amplitude").onChange((v) => grass.flowAmplitude = v);
simFolder.add(params, "grassFlowFrequency", 0.02, 1, 0.01).name("Flow Frequency").onChange((v) => grass.flowFrequency = v);
simFolder.add(params, "grassFlowSpeed", 0, 2, 0.01).name("Flow Speed").onChange((v) => grass.flowSpeed = v);
simFolder.add(params, "grassFlowPhaseOffset", 0, 2, 0.01).name("Flow Phase Offset").onChange((v) => grass.flowPhaseOffset = v);
simFolder.add(params, "grassFlowVerticalAmplitude", 0, 4, 0.01).name("Flow Vertical").onChange((v) => grass.flowVerticalAmplitude = v);
simFolder.add(params, "grassWaveAmplitude", 0, 4, 0.01).name("Wave Amplitude").onChange((v) => grass.waveAmplitude = v);
simFolder.add(params, "grassWaveSpeed", 0, 3, 0.01).name("Wave Speed").onChange((v) => grass.waveSpeed = v);
simFolder.add(params, "grassWavePhasePerJoint", 0, 3.14, 0.01).name("Wave Phase/Joint").onChange((v) => grass.wavePhasePerJoint = v);
simFolder.add(params, "grassSelfRight", 0, 3, 0.01).name("Self-Right").onChange((v) => grass.selfRight = v);
simFolder.add(params, "grassTiltAmplitude", 0, 1, 0.01).name("Rest Tilt").onChange((v) => {
  grass.tiltAmplitude = v;
  renderer.compute(grass.computeInit);
});
simFolder.add(params, "grassBladeWidth", 0.1, 3, 0.05).name("Width").onChange((v) => grass.bladeWidth = v);
simFolder.add(params, "grassBladeHeight", 0.1, 3, 0.05).name("Height").onChange((v) => grass.bladeHeight = v);
simFolder.add(params, "grassPushRadius", 0.1, 6, 0.05).name("Push Radius").onChange((v) => {
  grass.pushRadius = v;
  mouseColliderMesh.scale.setScalar(v);
});
simFolder.add(params, "mousePushStrength", 0, 30, 0.1).name("Mouse Push Strength").onChange((v) => grass.colliders[0].strength = v);
simFolder.add(params, "grassPushRecovery", 0.1, 5, 0.05).name("Push Recovery").onChange((v) => grass.pushRecovery = v);
simFolder.add(params, "raycastPlaneY", -2, 6, 0.01).name("Ray Plane Y").onChange((v) => groundPlane.constant = -v);
var matFolder = gui.addFolder("Anemone Material");
matFolder.addColor(params, "grassBaseColor").name("Base Color").onChange((v) => grass.bladeBaseColor = v);
var grassTipColorPresets = {
  Cyan: "#4f74b0",
  "Neon Pink": "#b04fad",
  "Dark Green": "#1c515f"
};
var tipColorController = matFolder.addColor(params, "grassTipColor").name("Tip Color").onChange((v) => grass.bladeTipColor = v);
var tipPresetProxy = { preset: "Cyan" };
matFolder.add(tipPresetProxy, "preset", Object.keys(grassTipColorPresets)).name("Tip Preset").onChange((name) => {
  const hex = grassTipColorPresets[name];
  params.grassTipColor = hex;
  grass.bladeTipColor = hex;
  tipColorController.updateDisplay();
});
matFolder.add(params, "grassGradientFalloff", 0.1, 5, 0.01).name("Gradient Falloff").onChange((v) => grass.bladeGradientFalloff = v);
matFolder.add(params, "grassGradientOffset", 0, 0.95, 0.01).name("Gradient Offset").onChange((v) => grass.bladeGradientOffset = v);
matFolder.add(params, "grassColorVariation", 0, 1, 0.01).name("Color Variation").onChange((v) => grass.bladeColorVariation = v);
matFolder.add(params, "grassRoughness", 0, 1, 0.01).name("Roughness").onChange((v) => grassMaterial.roughness = v);
matFolder.add(params, "grassMetalness", 0, 1, 0.01).name("Metalness").onChange((v) => grassMaterial.metalness = v);
matFolder.add(params, "grassClearcoat", 0, 1, 0.01).name("Clearcoat").onChange((v) => grassMaterial.clearcoat = v);
matFolder.add(params, "grassSheen", 0, 1, 0.01).name("Sheen").onChange((v) => grassMaterial.sheen = v);
matFolder.add(params, "grassIridescence", 0, 1, 0.01).name("Iridescence").onChange((v) => grassMaterial.iridescence = v);
matFolder.add(params, "grassIridescenceIOR", 1, 2.333, 0.01).name("Iridescence IOR").onChange((v) => grassMaterial.iridescenceIOR = v);
matFolder.add(params, "grassIridescenceThicknessMin", 0, 1200, 1).name("Iridescence Min (nm)").onChange((v) => {
  grassMaterial.iridescenceThicknessRange = [v, params.grassIridescenceThicknessMax];
});
matFolder.add(params, "grassIridescenceThicknessMax", 0, 1200, 1).name("Iridescence Max (nm)").onChange((v) => {
  grassMaterial.iridescenceThicknessRange = [params.grassIridescenceThicknessMin, v];
});
matFolder.addColor(params, "grassTipGlow").name("Tip Glow").onChange((v) => {
  grassMaterial.emissive.set(v);
  grassEmissiveColorU.value.set(v);
});
matFolder.add(params, "grassTipGlowIntensity", 0, 1, 0.01).name("Tip Glow Intensity").onChange((v) => {
  grassMaterial.emissiveIntensity = v;
  grassEmissiveIntensityU.value = v;
});
matFolder.add(params, "grassEmissiveNoiseEnabled").name("Emissive Noise").onChange((v) => grassEmissiveNoiseEnabledU.value = v ? 1 : 0);
matFolder.add(params, "grassEmissiveNoiseAmp", 0, 1, 0.01).name("Emissive Noise Amp").onChange((v) => grassEmissiveNoiseAmpU.value = v);
matFolder.add(params, "grassEmissiveNoiseFrequency", 0, 150, 0.1).name("Emissive Noise Freq").onChange((v) => grassEmissiveNoiseFreqU.value = v);
matFolder.add(params, "grassSpeckleEnabled").name("Speckles").onChange((v) => grassSpeckleEnabledU.value = v ? 1 : 0);
matFolder.add(params, "grassSpeckleAmp", 0, 5, 0.01).name("Speckle Amp").onChange((v) => grassSpeckleAmpU.value = v);
matFolder.add(params, "grassSpeckleFrequency", 0, 500, 0.1).name("Speckle Freq").onChange((v) => grassSpeckleFreqU.value = v);
matFolder.add(params, "grassSpeckleThreshold", 0, 1, 1e-3).name("Speckle Threshold").onChange((v) => grassSpeckleThresholdU.value = v);
matFolder.add(params, "grassTipGlowStart", 0, 1, 0.01).name("Tip Glow Start").onChange(() => {
  paintTipMask();
  tipMaskTex.needsUpdate = true;
});
matFolder.add(params, "grassTipGlowFalloff", 0.1, 8, 0.05).name("Tip Glow Falloff").onChange(() => {
  paintTipMask();
  tipMaskTex.needsUpdate = true;
});
var fishFolder = gui.addFolder("Fish");
fishFolder.add(params, "fishScale", 0.1, 5, 0.01).name("Scale").onChange(applyFishTransform);
fishFolder.add(params, "fishPathSpeed", -0.5, 0.5, 1e-3).name("Swim Speed");
fishFolder.add(params, "fishPathOffset", 0, 1, 1e-3).name("Path Offset").listen().onChange((v) => {
  if (pathFollow) pathFollow.pathOffset.value = v;
});
fishFolder.add(params, "showPathHelper").name("Show Path").onChange((v) => {
  if (pathHelper) pathHelper.visible = params.debug && v;
  if (pathPointsHelper) pathPointsHelper.visible = params.debug && v;
});
var pathPointGeo = new THREE4.SphereGeometry(0.01, 12, 8);
var pathPointMat = new THREE4.MeshBasicMaterial({ color: 65416, fog: false });
function rebuildPathPointsHelper() {
  if (!pathPointsHelper) return;
  for (const child of [...pathPointsHelper.children]) {
    pathPointsHelper.remove(child);
  }
  const pts = buildWigglePath();
  for (const p of pts) {
    const m = new THREE4.Mesh(pathPointGeo, pathPointMat);
    m.position.copy(p);
    pathPointsHelper.add(m);
  }
}
function rebuildWigglePath() {
  if (!pathFollow) return;
  pathFollow.setPoints(buildWigglePath());
  if (pathHelper) pathFollow.refreshHelper(pathHelper);
  rebuildPathPointsHelper();
}
fishFolder.add(params, "fishColliderRadiusScale", 0.1, 5, 0.01).name("Collider Radius").onChange(() => rebuildFishCollider());
fishFolder.add(params, "fishColliderLengthScale", 0.1, 5, 0.01).name("Collider Length").onChange(() => rebuildFishCollider());
fishFolder.add(params, "wiggleAmplitude", 0, 2, 0.01).name("Wiggle Amp").onChange(rebuildWigglePath);
fishFolder.add(params, "wiggleFrequency", 0, 5, 0.01).name("Wiggle Freq").onChange(rebuildWigglePath);
var pathPointsFolder = fishFolder.addFolder("Path Points");
for (let i = 0; i < pathPointCount; i++) {
  const sub = pathPointsFolder.addFolder(`Point ${i + 1}`);
  sub.add(params, `pathPoint${i}X`, -10, 10, 0.01).name("X").onChange(rebuildWigglePath);
  sub.add(params, `pathPoint${i}Y`, -2, 8, 0.01).name("Y").onChange(rebuildWigglePath);
  sub.add(params, `pathPoint${i}Z`, -10, 10, 0.01).name("Z").onChange(rebuildWigglePath);
}
var coralFolder = gui.addFolder("Corals");
coralFolder.add(params, "showCorals").name("Show Corals").onChange((v) => coralsGroup.visible = v);
for (const def of coralDefs) {
  const sub = coralFolder.addFolder(def.name);
  const onChange = () => {
    const inst = coralInstances.find((i) => i.id === def.id);
    if (inst) applyCoralTransform(inst);
  };
  sub.add(params, `${def.id}X`, -10, 10, 0.01).name("X").onChange(onChange);
  sub.add(params, `${def.id}Y`, -5, 5, 0.01).name("Y").onChange(onChange);
  sub.add(params, `${def.id}Z`, -10, 10, 0.01).name("Z").onChange(onChange);
  sub.add(params, `${def.id}Scale`, 0.1, 10, 0.01).name("Scale").onChange(onChange);
  sub.add(params, `${def.id}ColliderRadius`, 0.05, 5, 0.01).name("Collider Radius").onChange(onChange);
}
var particleFolder = gui.addFolder("Plankton");
particleFolder.add(params, "showPlankton").name("Show Plankton").onChange((v) => particles.visible = v);
particleFolder.add(params, "particleSizeMin", 0, 0.15, 1e-3).name("Size Min").onChange((v) => particleSizeMinU.value = v);
particleFolder.add(params, "particleSizeMax", 0, 0.15, 1e-3).name("Size Max").onChange((v) => particleSizeMaxU.value = v);
particleFolder.add(params, "particleSpeed", 0, 2, 0.01).name("Speed").onChange((v) => particleSpeedU.value = v);
particleFolder.addColor(params, "particleColor").name("Color").onChange((v) => particleColorU.value.set(v));
particleFolder.add(params, "particleOpacity", 0, 2, 0.01).name("Opacity").onChange((v) => particleOpacityU.value = v);
particleFolder.add(params, "particleStretchMin", 1, 4, 0.01).name("Stretch Min").onChange((v) => particleStretchMinU.value = v);
particleFolder.add(params, "particleStretchMax", 1, 4, 0.01).name("Stretch Max").onChange((v) => particleStretchMaxU.value = v);
particleFolder.add(params, "particleGranule", 0, 2, 0.01).name("Granule").onChange((v) => particleGranuleU.value = v);
particleFolder.add(params, "particleEdge", 0, 0.5, 5e-3).name("Edge Softness").onChange((v) => particleEdgeU.value = v);
particleFolder.add(params, "particleLighting", 0, 1, 0.01).name("Lighting (+Y)").onChange((v) => particleLightingU.value = v);
particleFolder.add(params, "particleVolumeXZ", 2, 30, 0.1).name("Volume XZ").onChange((v) => particleVolumeXZU.value = v);
particleFolder.add(params, "particleVolumeY", 1, 15, 0.1).name("Volume Y").onChange((v) => particleVolumeYU.value = v);
particleFolder.add(params, "particleVolumeYBase", -2, 5, 0.05).name("Volume Y Base").onChange((v) => particleVolumeYBaseU.value = v);
var raycaster = new THREE4.Raycaster();
var mouseNDC = new THREE4.Vector2();
var groundPlane = new THREE4.Plane(new THREE4.Vector3(0, 1, 0), -params.raycastPlaneY);
var mouseHitPoint = new THREE4.Vector3();
var PUSH_AWAY = new THREE4.Vector3(99999, 0, 99999);
var mouseOverCanvas = false;
renderer.domElement.addEventListener("pointermove", (e) => {
  mouseNDC.set(e.clientX / innerWidth * 2 - 1, -(e.clientY / innerHeight) * 2 + 1);
  raycaster.setFromCamera(mouseNDC, camera);
  if (raycaster.ray.intersectPlane(groundPlane, mouseHitPoint)) {
    mouseOverCanvas = true;
  }
});
renderer.domElement.addEventListener("pointerenter", () => {
  mouseOverCanvas = true;
});
renderer.domElement.addEventListener("pointerleave", () => {
  mouseOverCanvas = false;
});
var mouseColliderMesh = new THREE4.Mesh(
  new THREE4.SphereGeometry(1, 20, 14),
  new THREE4.MeshBasicMaterial({
    color: 65416,
    wireframe: true,
    fog: false,
    transparent: true,
    opacity: 0.5
  })
);
mouseColliderMesh.scale.setScalar(params.grassPushRadius);
mouseColliderMesh.visible = false;
scene.add(mouseColliderMesh);
addEventListener("resize", () => {
  camera.aspect = innerWidth / innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize(innerWidth, innerHeight);
  updateBackgroundPlane();
});
await renderer.computeAsync(grass.computeInit);
var capsuleA = new THREE4.Vector3();
var capsuleB = new THREE4.Vector3();
var lastT = performance.now() * 1e-3;
renderer.setAnimationLoop(() => {
  const now = performance.now() * 1e-3;
  const dt = Math.min(0.1, now - lastT);
  lastT = now;
  controls.update();
  if (pathFollow) {
    params.fishPathOffset = (params.fishPathOffset + dt * params.fishPathSpeed + 1) % 1;
    pathFollow.pathOffset.value = params.fishPathOffset;
  }
  if (mouseOverCanvas) {
    grass.colliders[0].position.copy(mouseHitPoint);
    mouseColliderMesh.position.copy(mouseHitPoint);
  } else {
    grass.colliders[0].position.copy(PUSH_AWAY);
  }
  mouseColliderMesh.visible = params.debug && params.showColliders && mouseOverCanvas;
  if (fishPivot?.userData.collider && pathFollow) {
    const colliderMesh = fishPivot.userData.collider;
    const curve = pathFollow.curve;
    const segment = pathFollow.pathSegment.value;
    const headT = ((params.fishPathOffset + segment) % 1 + 1) % 1;
    const tailT = (params.fishPathOffset % 1 + 1) % 1;
    fishPivot.updateMatrixWorld(true);
    curve.getPointAt(tailT, capsuleA).applyMatrix4(fishPivot.matrixWorld);
    curve.getPointAt(headT, capsuleB).applyMatrix4(fishPivot.matrixWorld);
    colliderMesh.position.copy(capsuleA).add(capsuleB).multiplyScalar(0.5);
    const dir = capsuleB.clone().sub(capsuleA);
    const length2 = dir.length();
    if (length2 > 1e-4) {
      dir.normalize();
      colliderMesh.quaternion.setFromUnitVectors(new THREE4.Vector3(0, 1, 0), dir);
    }
    grass.colliders[1].position.copy(capsuleA);
    grass.colliders[1].endpoint.copy(capsuleB);
    grass.colliders[1].radius = colliderMesh.userData.colliderRadius * fishPivot.scale.x;
  }
  grass.update();
  for (const inst of coralInstances) {
    inst.pivot.updateMatrixWorld(true);
    coralWorldPos.copy(inst.localSphereCenter).applyMatrix4(inst.pivot.matrixWorld);
    const slot = grass.colliders[inst.slotIdx];
    slot.position.copy(coralWorldPos);
    slot.endpoint.copy(coralWorldPos);
    const scale = params[`${inst.id}Scale`];
    const radius = params[`${inst.id}ColliderRadius`];
    slot.radius = radius * scale;
  }
  if (sun.map) proceduralCaustics.update();
  postProcessing.render();
  stats.update();
  renderer.resolveTimestampsAsync("render");
  renderer.resolveTimestampsAsync("compute");
});
