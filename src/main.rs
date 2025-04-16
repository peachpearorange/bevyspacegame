#![allow(clippy::unnecessary_cast)]
#![allow(unused_imports)]
#![allow(dead_code)]
#![feature(const_trait_impl)]
// #![feature(type_alias_impl_trait)]
#![allow(unused_mut)]
#![allow(non_camel_case_types)]
#![allow(non_upper_case_globals)]
#![feature(variant_count)]
#![feature(strict_overflow_ops)]
#![feature(iter_intersperse)]
#![feature(trivial_bounds)]
#![feature(impl_trait_in_assoc_type)]
// #![feature(option_get_or_insert_default)]
#![feature(let_chains)]
#![feature(const_closures)]
// #![feature(const_mut_refs)]

// #![feature(int_roundings)]
// #![recursion_limit = "1024"]
// #![feature(const_fn_floating_point_arithmetic)]

pub mod bundletree;
// pub mod ui;
mod aigen;
pub use bevy::prelude::Name;
use {aigen::*,
     bevy::{asset::LoadState,
            core_pipeline::Skybox,
            ecs::query::QueryData,
            render::render_resource::{Extent3d, TextureViewDescriptor}},
     bevy_sprite3d::Sprite3dParams};
use {avian3d::prelude::*,
     bevy::{app::AppExit,
            asset::{AssetServer, Handle},
            core_pipeline::bloom::{Bloom, BloomCompositeMode, BloomPrefilter},
            ecs::{entity::EntityHashMap, world::Command},
            image::ImageFilterMode,
            math::{Vec3, primitives, vec3},
            pbr::{CubemapVisibleEntities, StandardMaterial},
            prelude::*,
            render::primitives::CubemapFrusta,
            utils::{HashMap, HashSet},
            window::WindowMode},
     bevy_embedded_assets::*,
     // bevy_mod_billboard::{BillboardDepth, BillboardLockAxis, BillboardMeshHandle,
     //                      BillboardTextBundle, BillboardTextureBundle,
     //                      BillboardTextureHandle}
     // ,
     bevy_panorbit_camera::PanOrbitCamera,
     bevy_sprite3d::Sprite3dBuilder,
     // bevy_quill::{prelude::*, QuillPlugin, ViewChild},
     // bevy_quill_overlays::QuillOverlaysPlugin,
     dynamics::solver::SolverConfig,
     fancy_constructor::new,
     haalka::prelude::*,
     rand::{Rng, random, thread_rng},
     rust_utils::{comment, concat_strings, debugfmt, filter_map, find_map, map, println,
                  sum, vec},
     std::f32::consts::PI};

comment! {
  // Voxel Scenes
  pub static FLASHLIGHT: MyAsset<Scene> =
    MyAsset::path_with_label("flashlight.vox", "flashlight");
  pub static FLOWER: MyAsset<Scene> = MyAsset::path("flower.vox");
  pub static GLOW_TEST: MyAsset<Scene> = MyAsset::path("glowtest.vox");
}

pub const GLOWY_COLOR: Color = Color::srgb(13.99, 11.32, 50.0);
pub const GLOWY_COLOR_2: Color = Color::srgb(30.0, 20.7, 10.5);
pub const GLOWY_COLOR_3: Color = Color::srgb(0.0, 30.0, 0.0);
pub const EXPLOSION_COLOR: Color = Color::srgb(8.0, 3.0, 3.0);
pub const LASER_COLOR: Color = Color::hsv(60.0, 1.0, 4.0);
// hsv(61, 100%, 100%)
pub const BILLBOARD_REL_SCALE: f32 = 2.0;
pub const TEXT_SCALE: f32 = 0.013;
pub const ENABLE_SHADOWS_OTHER_THAN_SUN: bool = false;
pub const AMBIENT_LIGHT: AmbientLight =
  AmbientLight { color: Color::WHITE, brightness: 300.0 };
pub const BLOOM: Bloom = Bloom {
  intensity: 0.5,
  low_frequency_boost: 0.0,
  prefilter: BloomPrefilter { threshold: 2.2, threshold_softness: 0.0 },

  composite_mode: BloomCompositeMode::Additive,
  ..Bloom::NATURAL
};

const TONEMAPPING: bevy::core_pipeline::tonemapping::Tonemapping =
  bevy::core_pipeline::tonemapping::Tonemapping::Reinhard;

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct MySprite {
  path: &'static str
}
impl MySprite {
  const fn new(path: &'static str) -> Self { Self { path } }
  fn embedded_path(&self) -> String { format!("embedded://{}", self.path) }

  // to the ai making zones: if there is a GPT4O sprite for something, you should prefer to use that. Second best are the IMAGEN3 ones. Otherwise use one of the other ones.
  const GPT4O_GREEN_CAR_SHIP: Self = Self::new("4o green car ship.png");
  const GPT4O_MINING_SHIP: Self = Self::new("4o mining ship.png");
  const GPT4O_PIRATE_SHIP: Self = Self::new("4o pirate ship.png");
  const GPT4O_PIRATE_STATION: Self = Self::new("4o pirate station.png");
  const GPT4O_TRADING_STATION: Self = Self::new("4o trading station.png");
  const GPT4O_WHITE_EXPLORATION_SHIP: Self = Self::new("4o white exploration ship.png");
  const GPT4O_ASTEROID: Self = Self::new("gpt4o asteroid.png");
  const GPT4O_ICE_ASTEROID: Self = Self::new("gpt4o ice asteroid.png");
  const GPT4O_POLICE_SPACE_SHIP: Self = Self::new("gpt4o police space ship.png");
  const GPT4O_SPACE_CAT: Self = Self::new("gpt4o space cat.png");
  const GPT4O_SPACE_MAN: Self = Self::new("gpt4o space man.png");
  const GPT4O_SPHERICAL_COW: Self = Self::new("gpt4o spherical cow.png");
  const GPT4O_PLANET_ARRAKIS: Self = Self::new("GPT4O Arrakis.png");
  const GPT4O_PLANET_MUSTAFAR: Self = Self::new("GPT4O Mustafar.png");
  const GPT4O_PLANET_TRANTOR: Self = Self::new("GPT4O Trantor.png");
  const GPT4O_CONTAINER: Self = Self::new("GPT4O Container.png");
  const GPT4O_GATE: Self = Self::new("GPT4O gate.png");
  const IMAGEN3GREENSPACESHIP: Self = Self::new("imagen3greenspaceship.png");
  const IMAGEN3WIZARDSPACESHIP: Self = Self::new("imagen3wizardspaceship.png");
  const IMAGEN3WHITESPACESHIP: Self = Self::new("imagen3whitespaceship.png");
  const IMAGEN3SPACESTATION: Self = Self::new("imagen3spacestation.png");
  const IMAGEN3FLOATINGISLAND: Self = Self::new("imagen3floatingisland.png");
  const ASTEROID: Self = Self::new("asteroid.png");
  const BLOCKTEXTURES: Self = Self::new("pixelc/block_textures.png");
  const BRICKS: Self = Self::new("pixelc/bricks.png");
  const BROWNGASGIANT: Self = Self::new("pixelc/browngasgiant.png");
  const CAR: Self = Self::new("car.png");
  const CHEST: Self = Self::new("pixelc/chest.png");
  const COFFEE: Self = Self::new("coffee.png");
  const COIN: Self = Self::new("coin.png");
  const CONTAINER: Self = Self::new("container.png");
  const CRYSTALASTEROID: Self = Self::new("crystal_asteroid.png");
  const CRYSTALMONSTER: Self = Self::new("crystal_monster.png");
  const DESERT_PLANET_IMAGEN_3: Self = Self::new("desert_planet_imagen_3.png");
  const FIRE: Self = Self::new("fire.png");
  const FLOATINGISLAND: Self = Self::new("floating_island.png");
  const GATE: Self = Self::new("gate.png");
  const GRASS: Self = Self::new("grass.png");
  const GROUND: Self = Self::new("ground.png");
  const HABITABLEPLANET: Self = Self::new("pixelc/habitableplanet.png");
  const HPBOX: Self = Self::new("hpbox.png");
  const ICEASTEROID: Self = Self::new("icesteroid.png");
  const ICEBERG: Self = Self::new("iceberg.png");
  const ICEPLANET: Self = Self::new("ice_planet.png");
  const LAVAPLANET: Self = Self::new("lava_planet.png");
  const MARSLIKEPLANET: Self = Self::new("pixelc/marslikeplanet.png");
  const MISSILE: Self = Self::new("pixelc/missile.png");
  const MUSHROOMMAN: Self = Self::new("mushroom_man.png");
  const NASASTARMAP: Self = Self::new("nasa_starmap.jpg");
  const NOTE: Self = Self::new("note.png");
  const PENGUIN: Self = Self::new("penguin.png");
  const PLAYER: Self = Self::new("player.png");
  const PURPLEENEMYSHIP: Self = Self::new("purpleenemyship.png");
  const SANDPLANET: Self = Self::new("sandplanet.png");
  const SIGN: Self = Self::new("sign.png");
  const SKYBOX: Self = Self::new("skybox.jpeg");
  const SKYBOXMANYWORLDSSPACE: Self = Self::new("skyboxmanyworldsspace.jpg");
  const SNOW: Self = Self::new("snow.png");
  const SPACECAT: Self = Self::new("space_cat.png");
  const SPACECOWBOY: Self = Self::new("spacecowboy.png");
  const SPACEMAN: Self = Self::new("spaceman.png");
  const SPACEPIRATEBASE: Self = Self::new("spacepiratebase.png");
  const SPACESHIPABANDONED: Self = Self::new("spaceshipabandoned.png");
  const SPACESHIPBLUE: Self = Self::new("spaceshipblue.png");
  const SPACESHIPDARKRED: Self = Self::new("spaceshipdarkred.png");
  const SPACESHIPGREEN: Self = Self::new("spaceshipgreen.png");
  const SPACESHIPPURPLE: Self = Self::new("spaceshippurple.png");
  const SPACESHIPRED: Self = Self::new("spaceshipred.png");
  const SPACESHIPWHITE2: Self = Self::new("spaceshipwhite2.png");
  const SPACESHIPWHITE: Self = Self::new("spaceshipwhite.png");
  const SPACESTATION: Self = Self::new("space_station.png");
  const SPACEWIZARD: Self = Self::new("spacewizard.png");
  const SPHERICALCOW: Self = Self::new("spherical_cow.png");
  const STICKMAN: Self = Self::new("stickman.png");
  const STONE: Self = Self::new("stone.png");
  const SUN: Self = Self::new("sun.png");
  const TENT: Self = Self::new("tent.png");
  const TORCH: Self = Self::new("pixelc/torch.png");
  const TREE: Self = Self::new("tree.png");
  const TREEMONSTER: Self = Self::new("treemonster.png");
  const TURRET: Self = Self::new("turret.png");
  const WATER: Self = Self::new("water.png");
  const WHITECORNERS: Self = Self::new("white_corners.png");
  const WIZARDSPACESHIP: Self = Self::new("wizardspaceship.png");
  const WORMHOLE: Self = Self::new("wormhole.png");
  const ZORP: Self = Self::new("zorp.png");
}
#[derive(Copy, Clone, Hash, Eq, PartialEq)]
struct MyImageMaterial {
  img: MySprite,
  mat_fn: fn(Handle<Image>) -> StandardMaterial
}
impl MyImageMaterial {
  const fn new(mat_fn: fn(Handle<Image>) -> StandardMaterial, img: MySprite) -> Self {
    Self { img, mat_fn }
  }
  pub fn val(&self, h: Handle<Image>) -> StandardMaterial { (self.mat_fn)(h) }
  pub fn img(&self) -> MySprite { self.img }
  const GROUND: Self = Self::new(
    |h| StandardMaterial {
      perceptual_roughness: 0.8,
      metallic: 0.0,
      reflectance: 0.2,
      ..h.into()
    },
    MySprite::GROUND
  );
  const SNOW: Self = Self::new(
    |h| StandardMaterial {
      perceptual_roughness: 0.4,
      metallic: 0.0,
      reflectance: 0.5,
      ior: 1.31,
      ..h.into()
    },
    MySprite::SNOW
  );
  const WATER: Self = Self::new(
    |h| StandardMaterial {
      perceptual_roughness: 0.3,
      metallic: 0.0,
      reflectance: 0.5,
      ..h.into()
    },
    MySprite::WATER
  );
  const STONE: Self = Self::new(
    |h| StandardMaterial {
      perceptual_roughness: 0.8,
      metallic: 0.0,
      reflectance: 0.3,
      ..h.into()
    },
    MySprite::STONE
  );
  const BRICKS: Self = Self::new(
    |h| StandardMaterial {
      perceptual_roughness: 0.95,
      metallic: 0.0,
      reflectance: 0.1,
      ..h.into()
    },
    MySprite::BRICKS
  );
  const GRASS: Self = Self::new(
    |h| StandardMaterial {
      perceptual_roughness: 0.8,
      metallic: 0.0,
      reflectance: 0.2,
      ..h.into()
    },
    MySprite::GRASS
  );
  const PENGUIN: Self = Self::new(From::from, MySprite::PENGUIN);
}
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct MyMaterial {
  mat_fn: fn() -> StandardMaterial
}
impl MyMaterial {
  const fn new(mat_fn: fn() -> StandardMaterial) -> Self { Self { mat_fn } }
  pub fn val(&self) -> StandardMaterial { (self.mat_fn)() }
  const GLOWY: Self = Self::new(|| StandardMaterial {
    unlit: true,
    alpha_mode: AlphaMode::Mask(0.0),
    ..GLOWY_COLOR.into()
  });
  const GLOWY_2: Self = Self::new(|| StandardMaterial {
    unlit: true,
    alpha_mode: AlphaMode::Mask(0.0),
    ..GLOWY_COLOR_2.into()
  });
  const GLOWY_3: Self = Self::new(|| StandardMaterial {
    unlit: true,
    alpha_mode: AlphaMode::Mask(0.0),
    ..GLOWY_COLOR_3.into()
  });
  const EXPLOSION: Self = Self::new(|| StandardMaterial {
    unlit: true,
    alpha_mode: AlphaMode::Mask(0.0001),
    ..EXPLOSION_COLOR.into()
  });
  const LASER: Self = Self::new(|| StandardMaterial {
    unlit: true,
    alpha_mode: AlphaMode::Mask(0.0001),
    ..LASER_COLOR.into()
  });
  const PARTICLE: Self = Self::new(|| StandardMaterial::from(Color::srgb(0.2, 0.7, 0.9)));
  const INVISIBLE: Self = Self::new(|| StandardMaterial {
    unlit: true,
    alpha_mode: AlphaMode::Blend,
    ..Color::srgba(0.0, 0.0, 0.0, 0.0).into()
  });
  const HOVERED: Self = Self::new(|| StandardMaterial {
    unlit: true,
    alpha_mode: AlphaMode::Blend,
    ..Color::srgba(0.0, 0.3, 1.0, 0.1).into()
  });
  const PRESSED: Self = Self::new(|| StandardMaterial {
    unlit: true,
    alpha_mode: AlphaMode::Blend,
    ..Color::srgba(0.0, 0.3, 1.0, 0.3).into()
  });
  const SELECTED: Self = Self::new(|| StandardMaterial {
    unlit: true,
    alpha_mode: AlphaMode::Blend,
    ..Color::srgba(0.0, 0.3, 1.0, 0.2).into()
  });
}
#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct MyScene {
  path: &'static str,
  label: &'static str
}
impl MyScene {
  pub const fn new(path: &'static str, label: &'static str) -> Self { Self { path, label } }
  pub const fn path_and_label(&self) -> (&'static str, &'static str) {
    (self.path, self.label)
  }
  pub const LUNAR_LANDER: Self = Self::new("lunarlander.glb", "Scene0");
  pub const CHARACTER_CONTROLLER_DEMO: Self =
    Self::new("character_controller_demo.glb", "Scene0");
  pub const LEVEL: Self = Self::new("level.glb", "Scene0");
  pub const A_LEVEL: Self = Self::new("alevel.gltf", "Scene0");
  pub const ISLAND_LEVEL: Self = Self::new("this_here_level.glb", "Scene0");
  pub const SOME_SKETCH_LEVEL: Self = Self::new("somesketchlevel.glb", "Scene0");
  pub const SNOWMAN: Self = Self::new("snowman.glb", "Scene0");
  pub const COFFEE_SCENE: Self = Self::new("coffee.glb", "Scene0");
  pub const GOXEL_LEVEL: Self = Self::new("goxel_level.glb", "Scene0");
  pub const TURTLE_LEVEL: Self = Self::new("turtle level.gltf", "Scene0");
  pub const WAT: Self = Self::new("wat.glb", "Scene0");
}
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct GenMesh {
  gen_fn: fn() -> Mesh
}
impl GenMesh {
  const fn new(gen_fn: fn() -> Mesh) -> Self { Self { gen_fn } }
  pub fn generate(&self) -> Mesh { (self.gen_fn)() }
  pub const UNIT_CUBE: Self = Self::new(|| Cuboid::new(1.0, 1.0, 1.0).into());
  pub const UNIT_CYLINDER: Self = Self::new(|| primitives::Cylinder::new(1.0, 1.0).into());
  pub const CUBE: Self = Self::new(|| Cuboid::new(0.7, 0.7, 0.7).into());
  pub const BOX: Self = Self::new(|| Cuboid::new(2.0, 1.0, 1.0).into());
  pub const FLAT_BOX: Self = Self::new(|| Cuboid::new(2.1, 0.3, 2.1).into());
  pub const CAPSULE: Self = Self::new(|| primitives::Capsule3d::default().into());
  pub const TORUS: Self = Self::new(|| primitives::Torus::default().into());
  pub const SPHERE: Self = Self::new(|| primitives::Sphere { radius: 1.0 }.into());
  pub const PLANE_SIZE_50: Self = Self::new(|| Cuboid::new(25.0, 0.1, 25.0).into());
  pub const BILLBOARD_MESH_SQUARE: Self = Self::new(|| {
    primitives::Rectangle::new(BILLBOARD_REL_SCALE, BILLBOARD_REL_SCALE).into()
  });
}
const min: fn(f32, f32) -> f32 = f32::min;

#[derive(Component, Clone, PartialEq, Eq)]
pub struct TextDisplay(pub String);

impl<T: ToString> From<T> for TextDisplay {
  fn from(value: T) -> Self { Self(value.to_string()) }
}

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug)]
pub enum Visuals {
  Sprite(MySprite),
  MaterialMesh { material: MyMaterial, mesh: GenMesh } // None
}
// #[derive(Component, Clone, PartialEq, Eq, Default)]
// pub struct Visuals {
//   text: Option<String>,
//   done: bool,
//   material_mesh: Option<(MyMaterial, GenMesh)>,
//   shield_active: bool,
//   desired_sprite: Option<MySprite>,
//   current_sprite: Option<MySprite>,
//   targeted: bool
// }

impl Visuals {
  // fn none() -> Self{Self::None}
  const fn sprite(sprite: MySprite) -> Self { Self::Sprite(sprite) }
  const fn material_mesh(material: MyMaterial, mesh: GenMesh) -> Self {
    Self::MaterialMesh { material, mesh }
  }
  const fn material_sphere(material: MyMaterial) -> Self {
    Self::material_mesh(material, GenMesh::SPHERE)
  }
  // fn with_text(self, text: impl ToString) -> Self {
  //   Self { text: Some(text.to_string()),
  //          ..self }
  // }
}
comment! {
  pub fn set_visuals(mut visuals_q: Query<(Entity, &mut Visuals, Option<&Combat>)>,
                     player: Single<&Player>) {
    for (e, mut visuals, ocombat) in &mut visuals_q {
      let should_have_target = player.target() == Some(e);
      // let new_visuals = Visuals{targeted:should_have_target,}
      let has_target = visuals.as_ref().targeted;
      if has_target != should_have_target {
        visuals.targeted = should_have_target;
      }
    }
  }
}
#[derive(Component, Clone)]
pub struct VisualSprite;
pub fn visuals(
  camq: Single<&GlobalTransform, With<Camera3d>>,
  serv: Res<AssetServer>,
  mut c: Commands,
  mut n: Local<u32>,
  mut j: Local<u32>,
  // images: Res<Assets<Image>>,
  mut target_entity: Local<Option<Entity>>,
  mut visuals_q: Query<(Entity, Mut<Visuals>, &Transform, Option<&Mesh3d>)>,
  // mut visuals_sprites_q: Query<(&mut Transform, &GlobalTransform),
  //       With<VisualSprite>>,
  mut option_target_overlay_entity: Local<Option<Entity>>,
  mut sprite_params: Sprite3dParams,
  mut sprite_handles: Local<HashMap<MySprite, Handle<Image>>>,
  mut mesh_handles: Local<HashMap<GenMesh, Handle<Mesh>>>,
  mut material_handles: Local<HashMap<MyMaterial, Handle<StandardMaterial>>>,
  mut visual_child_entities: Local<EntityHashMap<Entity>>,
  mut multi_visual_child_entities: Local<EntityHashMap<Entity>>
) {
  let mut get_material_handle = |material: MyMaterial| {
    material_handles.entry(material).or_insert_with(|| serv.add(material.val())).clone()
  };
  let mut get_mesh_handle = |mesh: GenMesh| {
    mesh_handles.entry(mesh).or_insert_with(|| serv.add(mesh.generate())).clone()
  };
  // let mut get_sprite_handle = |sprite @ MySprite { path }| {
  //   println!("d");
  //   if sprite_handles.get(&sprite).is_none() {
  //     println!("e");
  //     let embeddedpath = format!("embedded://{path}");
  //     dbg!(&embeddedpath);
  //     println!("c");
  //     let h = serv.load(embeddedpath);
  //     sprite_handles.insert(sprite, h);
  //   }
  //   let h = sprite_handles.get(&sprite).unwrap();
  //   dbg!(&h);
  //   h
  //   // sprite_handles.entry(sprite)
  //   //               .or_insert_with(|| {
  //   //                 let embeddedpath = format!("embedded://{path}");
  //   //                 dbg!(&embeddedpath);
  //   //                 println!("c");
  //   //                 serv.load(embeddedpath)
  //   //               })
  //   //               .clone()
  // };
  // let target_overlay = get_sprite_handle(MySprite::WHITECORNERS);
  let billboard_mesh_handle = get_mesh_handle(GenMesh::BILLBOARD_MESH_SQUARE);
  let invisible_material = get_material_handle(MyMaterial::INVISIBLE);
  // Children
  comment! {
    let target_entity = (*target_entity).unwrap_or_else(||{
      c.spawn((BillboardLockAxis { y_axis: false,
                                   rotation: false },
               BillboardTextureBundle { mesh:
                                        BillboardMeshHandle(billboard_mesh_handle),
                                        texture:
                                        BillboardTextureHandle(target_overlay),
                                        billboard_depth: BillboardDepth(false),
                                        transform:
                                        Transform::from_scale(Vec3::splat(1.7)),
                                        ..default() })).id()
    });
  }

  let text_color = TextColor::from(Color::WHITE);
  let text_font = TextFont { font: default(), font_size: 30.0, font_smoothing: default() };

  for (e, mut visuals, transform, omesh) in &mut visuals_q {
    let is_done = omesh.is_some();
    if !is_done {
      match *visuals {
        Visuals::Sprite(sprite) => {
          let embeddedpath = sprite.embedded_path();
          // dbg!(&embeddedpath);
          let h = serv.load(embeddedpath);
          if let Some(img) = sprite_params.images.get(h.id()) {
            let height = img.height();
            let scale = transform.scale.x;
            let pixels_per_metre = height as f32 / 2.5;
            // let pixels_per_metre = 0.02;
            let sprite_builder = Sprite3dBuilder {
              image: h,
              pixels_per_metre,
              alpha_mode: AlphaMode::Blend,
              unlit: true,
              double_sided: true,
              ..default()
            };
            let bundle = sprite_builder.bundle(&mut sprite_params);
            c.entity(e).insert(bundle);
            // visuals.done = true;
            *j += 1;
          } else {
            c.spawn(Sprite::from_image(h));
          }
        }
        Visuals::MaterialMesh { material, mesh } => {
          let material = get_material_handle(material);
          let mesh_handle = get_mesh_handle(mesh);
          c.entity(e).insert((Mesh3d(mesh_handle), MeshMaterial3d(material)));
          // visuals.done = true;
          // println("created material mesh");
        }
      }
    }
  }
}

#[derive(Component)]
pub enum FacingMode {
  Position,
  PositionIgnoreY,
  Direction
}

pub fn face_camera_system(
  camera_q: Query<&Transform, With<Camera3d>>,
  mut facers_q: Query<(&mut Transform, &GlobalTransform, &FacingMode), Without<Camera3d>>
) {
  if let Ok(cam_transform) = camera_q.get_single() {
    for (mut transform, global, mode) in &mut facers_q {
      let direction: Vec3 = match mode {
        FacingMode::PositionIgnoreY => {
          (global.translation() - cam_transform.translation).with_y(0.0)
        }
        FacingMode::Position => global.translation() - cam_transform.translation,
        FacingMode::Direction => cam_transform.forward().as_vec3()
      };
      transform.look_to(direction, Vec3::Y);
    }
  }
}
#[derive(Clone, Debug)]
pub struct PlayerTargetInteractionState {
  target: Entity,
  approaching: bool,
  shooting: bool,
  in_dialogue: bool
}

#[derive(Component, Clone, Debug, Default)]
pub struct Player {
  pub target_interaction_state: Option<PlayerTargetInteractionState> // pub object_interaction_minigame_state: Option<ObjectInteractionMiniGameState>
}

impl Player {
  pub fn set_target(&mut self, target: Entity) {
    self.target_interaction_state = Some(PlayerTargetInteractionState {
      target,
      shooting: false,
      approaching: false,
      in_dialogue: false
    });
  }
  pub fn untarget(&mut self) { self.target_interaction_state = None; }
  pub fn target(&self) -> Option<Entity> {
    self
      .target_interaction_state
      .as_ref()
      // .clone()
      .map(|&PlayerTargetInteractionState { target, .. }| target)
  }
}

pub fn insert_component<C: Component>(world: &mut World, entity: Entity, component: C) {
  if let Ok(mut entity_mut) = world.get_entity_mut(entity) {
    entity_mut.insert(component);
  }
}

pub fn update_component<C: Component + Clone>(
  world: &mut World,
  entity: Entity,
  f: impl FnOnce(C) -> C
) {
  // Children
  if let Ok(mut entity_mut) = world.get_entity_mut(entity)
    && let Some(mut component) = entity_mut.get_mut::<C>()
    && let updated = f((*component).clone())
  {
    *component = updated;
  }
}

pub fn mutate_component<C: Component>(
  world: &mut World,
  entity: Entity,
  f: impl FnOnce(&mut C)
) {
  if let Ok(mut entity_mut) = world.get_entity_mut(entity)
    && let Some(mut component) = entity_mut.get_mut::<C>()
  {
    f(&mut component);
  }
}

pub fn get_player(world: &mut World) -> Option<Entity> {
  world.query_filtered::<Entity, With<Player>>().iter(world).next()
}

// #[derive(Clone)]
pub struct MyCommand(pub Box<dyn FnOnce(&mut World) + 'static + Send + Sync>);

// impl From<Box<dyn FnOnce(&mut World) + 'static + Send + Sync>> for MyCommand {
//   fn from(f: Box<dyn FnOnce(&mut World) + 'static + Send + Sync>) -> Self { MyCommand(f) }
// }

// impl<F> From<F> for MyCommand where F: FnOnce(&mut World) + 'static + Send + Sync {
//   fn from(f: F) -> Self { MyCommand(Box::new(f)) }
// }

impl<F> From<F> for MyCommand
where
  F: FnOnce(&mut World) + 'static + Send + Sync
{
  fn from(f: F) -> Self { MyCommand(Box::new(f)) }
}
impl MyCommand {
  pub fn none() -> Self { (|_world: &mut World| {}).into() }

  pub fn multi(commands: impl IntoIterator<Item = MyCommand>) -> Self {
    let v = vec(commands);
    (move |world: &mut World| {
      for command in v {
        command.0(world);
      }
    })
    .into()
  }

  // pub fn spawn(b: impl Bundle) -> Self {
  //   let spawnable: Spawnable = b.into();
  //   (move |world: &mut World| {
  //     let mut commands = world.commands();
  //     spawnable.spawn(&mut commands);
  //   }).into()
  // }

  pub fn give_item_to_player(item: Item) -> Self {
    (move |world: &mut World| {
      if let Some(player_entity) = get_player(world) {
        mutate_component(world, player_entity, |inventory: &mut Inventory| {
          inventory.add_contents([(item.clone(), 1)]);
        });
      }
    })
    .into()
  }

  pub fn end_object_interaction_mini_game() -> Self {
    (|_world: &mut World| {
      // Implement mini-game ending logic here
    })
    .into()
  }

  pub fn damage_entity(entity: Entity, amount: u32) -> Self {
    (move |world: &mut World| {
      if let Some(mut combat) = world.get_mut::<Combat>(entity) {
        combat.hp = combat.hp.saturating_sub(amount);
      }
    })
    .into()
  }

  pub fn message_add(message: impl Into<String> + Send + Sync + 'static) -> Self {
    (move |world: &mut World| {
      let s: String = message.into();
      MESSAGE_LOG.update_mut(|v| v.push(Message::from(s)));
      // if let Some(mut ui_data) = world.get_resource_mut::<UIData>() {
      //   ui_data.message_add(message.to_string().clone());
      // }
    })
    .into()
  }

  pub fn despawn_entity(entity: Entity) -> Self {
    (move |world: &mut World| {
      world.commands().entity(entity).despawn_recursive();
    })
    .into()
  }
  pub fn despawn(entity: Entity) -> Self {
    (move |world: &mut World| {
      world.commands().entity(entity).despawn_recursive();
    })
    .into()
  }

  pub fn insert_component<C: Component + 'static>(entity: Entity, component: C) -> Self {
    (move |world: &mut World| insert_component(world, entity, component)).into()
  }

  pub fn update_component<C: Component + Clone + 'static>(
    entity: Entity,
    f: impl FnOnce(C) -> C + 'static + Send + Sync
  ) -> Self {
    (move |world: &mut World| update_component(world, entity, f)).into()
  }

  pub fn mutate_component<C: Component + 'static>(
    entity: Entity,
    f: impl FnOnce(&mut C) + 'static + Send + Sync
  ) -> Self {
    (move |world: &mut World| mutate_component(world, entity, f)).into()
  }

  pub fn insert_player_component<C: Component + 'static>(component: C) -> Self {
    (move |world: &mut World| {
      if let Some(player_entity) = get_player(world) {
        insert_component(world, player_entity, component);
      }
    })
    .into()
  }

  pub fn update_player_component<C: Component + Clone + 'static>(
    f: impl FnOnce(C) -> C + 'static + Send + Sync
  ) -> Self {
    (move |world: &mut World| {
      if let Some(player_entity) = get_player(world) {
        update_component(world, player_entity, f);
      }
    })
    .into()
  }

  pub fn mutate_player_component<C: Component + Clone + 'static>(
    f: impl FnOnce(&mut C) + 'static + Send + Sync
  ) -> Self {
    (move |world: &mut World| {
      if let Some(player_entity) = get_player(world) {
        mutate_component(world, player_entity, f);
      }
    })
    .into()
  }
}

impl Command for MyCommand {
  fn apply(self, world: &mut World) { (self.0)(world); }
}

// to the ai: implement all this stuff as extension methods of World. use auto-implementation for as many of them as possible. rust has a feature that lets you specify the implementation of methods in the trait definition. use it. one method should just return an &mut World. that method should have an implementation separate from the trait definition but all the other methods should use auto-implementation

pub trait WorldExt {
  fn get_world_mut(&mut self) -> &mut World;
  fn insert_component<C: Component + 'static>(&mut self, entity: Entity, component: C) {
    if let Ok(mut entity_mut) = self.get_world_mut().get_entity_mut(entity) {
      entity_mut.insert(component);
    }
  }
  fn update_component<C: Component + Clone + 'static>(
    &mut self,
    entity: Entity,
    f: impl FnOnce(C) -> C
  ) {
    if let Ok(mut entity_mut) = self.get_world_mut().get_entity_mut(entity)
      && let Some(mut component) = entity_mut.get_mut::<C>()
      && let updated = f((*component).clone())
    {
      *component = updated;
    }
  }
  fn mutate_component<C: Component + 'static>(
    &mut self,
    entity: Entity,
    f: impl FnOnce(&mut C)
  ) {
    if let Ok(mut entity_mut) = self.get_world_mut().get_entity_mut(entity)
      && let Some(mut component) = entity_mut.get_mut::<C>()
    {
      f(&mut component);
    }
  }
  fn get_player(&mut self) -> Option<Entity> {
    self
      .get_world_mut()
      .query_filtered::<Entity, With<Player>>()
      .iter(self.get_world_mut())
      .next()
  }
  fn give_items_to_player(&mut self, items: impl IntoIterator<Item = (Item, u32)>) {
    if let Some(player_entity) = self.get_player() {
      self.mutate_component(player_entity, |inventory: &mut Inventory| {
        inventory.add_contents(items);
      });
    }
  }
  fn give_item_to_player(&mut self, item: Item) {
    if let Some(player_entity) = self.get_player() {
      self.mutate_component(player_entity, |inventory: &mut Inventory| {
        inventory.add_contents([(item.clone(), 1)]);
      });
    }
  }
  fn end_object_interaction_mini_game(&mut self) {
    // Implement mini-game ending logic here
  }
  fn damage_entity(&mut self, entity: Entity, amount: u32) {
    if let Some(mut combat) = self.get_world_mut().get_mut::<Combat>(entity) {
      combat.hp = combat.hp.saturating_sub(amount);
    }
  }
  fn message_add(&mut self, message: impl Into<String> + Send + Sync + 'static) {
    let s: String = message.into();
    MESSAGE_LOG.update_mut(|v| v.push(Message::from(s)));
    // if let Some(mut ui_data) = self.get_world_mut().get_resource_mut::<UIData>() {
    //     ui_data.message_add(message.to_string().clone());
    // }
  }
  fn despawn_entity(&mut self, entity: Entity) {
    self.get_world_mut().commands().entity(entity).despawn_recursive();
  }
  fn despawn(&mut self, entity: Entity) {
    self.get_world_mut().commands().entity(entity).despawn_recursive();
  }
  fn insert_player_component<C: Component + 'static>(&mut self, component: C) {
    if let Some(player_entity) = self.get_player() {
      self.insert_component(player_entity, component);
    }
  }
  fn update_player_component<C: Component + Clone + 'static>(
    &mut self,
    f: impl FnOnce(C) -> C
  ) {
    if let Some(player_entity) = self.get_player() {
      self.update_component(player_entity, f);
    }
  }
  fn mutate_player_component<C: Component + Clone + 'static>(
    &mut self,
    f: impl FnOnce(&mut C)
  ) {
    if let Some(player_entity) = self.get_player() {
      self.mutate_component(player_entity, f);
    }
  }
}
impl WorldExt for World {
  fn get_world_mut(&mut self) -> &mut World { self }
}

// You would then use these extension methods like this:
// fn some_system(world: &mut World, player_entity: Entity) {
//     world.insert_component(player_entity, Inventory { contents: vec![] });
//     world.give_item_to_player(Item {});
//     world.message_add("You received an item!");
// }

// fn combat_actions()

#[derive(Component, Clone)]
pub enum VisualEffect {
  Laser { target: Entity, shooter: Entity },
  MISSILE { target: Entity, init_pos: Vec3 },
  Explosion { pos: Vec3 }
}

const LASER_DURATION_TICKS: u32 = 78;
const LASER_DAMAGE: u32 = 10;

impl VisualEffect {
  fn specify_transform(
    &self,
    query: &Query<&Transform, Without<VisualEffect>>,
    age: u32
  ) -> Option<Transform> {
    match *self {
      VisualEffect::Laser { target, shooter } => {
        let laser_age = age;
        let time_left = LASER_DURATION_TICKS - laser_age;
        // let shooter_transform = query.get(shooter)?;
        // let Ok() = query.get(target)?;
        //   && time_left > 0
        if let Ok(shooter_transform) = query.get(shooter)
          && let Ok(target_transform) = query.get(target)
          && time_left > 0
          && let start_pos = shooter_transform.translation
          && let target_pos = target_transform.translation
          && let distance = start_pos.distance(target_pos)
          && let center_pos = (start_pos + target_pos) * 0.5
          && let max_laser_radius = 0.18
          && let laser_radius = max_laser_radius
            * f32::sin(PI * time_left as f32 / LASER_DURATION_TICKS as f32).powf(0.4)
        {
          Some(
            Transform::from_translation(center_pos)
              .looking_at(target_pos, Vec3::Y)
              .with_scale(vec3(laser_radius, laser_radius, distance * 0.5))
          )
        } else {
          None
        }
      }
      VisualEffect::MISSILE { target, init_pos } => {
        let missile_travel_time_ticks = 100;
        let missile_age = age;
        let frac = missile_age as f32 / missile_travel_time_ticks as f32;
        if let Ok(&target_transform) = query.get(target)
          && frac < 1.0
        {
          let target_pos = target_transform.translation;
          Some(Transform::from_translation(init_pos.lerp(target_pos, frac)))
        } else {
          None
        }
      }
      VisualEffect::Explosion { pos } => {
        let explosion_max_time_ticks = 160;
        let explosion_age = age;
        let frac = explosion_age as f32 / explosion_max_time_ticks as f32;
        let scale = Vec3::splat(0.8 + (frac * 3.0));
        (frac < 1.0).then_some(Transform::from_translation(pos).with_scale(scale))
      }
    }
  }
}
pub fn explosion_visual(pos: Vec3, scale: f32) -> impl Bundle {
  (
    VisualEffect::Explosion { pos },
    Visuals::material_sphere(MyMaterial::EXPLOSION),
    Transform::default(),
    Visibility::Visible
  )
}

#[derive(Component)]
struct VisualEffectBox(
  Box<
    dyn Fn(&Query<&Transform, Without<VisualEffect>>, u32) -> Option<Transform>
      + Send
      + Sync
  >
);

pub fn laser_visual_new(shooter: Entity, target: Entity) -> impl Bundle {
  (
    Visuals::material_mesh(MyMaterial::LASER, GenMesh::SPHERE),
    Transform::default(),
    Visibility::Visible,
    VisualEffectBox(Box::new(move |q, age| {
      let laser_age = age;
      let time_left = LASER_DURATION_TICKS - laser_age;
      if let Ok(shooter_transform) = q.get(shooter)
        && let Ok(target_transform) = q.get(target)
        && time_left > 0
      {
        let start_pos = shooter_transform.translation;
        let target_pos = target_transform.translation;
        let distance = start_pos.distance(target_pos);
        let center_pos = (start_pos + target_pos) * 0.5;
        let max_laser_radius = 0.18;
        let laser_radius = max_laser_radius
          * f32::sin(PI * time_left as f32 / LASER_DURATION_TICKS as f32).powf(0.4);

        Some(
          Transform::from_translation(center_pos)
            .looking_at(target_pos, Vec3::Y)
            .with_scale(vec3(laser_radius, laser_radius, distance * 0.5))
        )
      } else {
        None
      }
    }))
  )
}
pub fn laser_visual(shooter: Entity, target: Entity) -> impl Bundle {
  (
    VisualEffect::Laser { target, shooter },
    Visuals::material_mesh(MyMaterial::LASER, GenMesh::SPHERE),
    Transform::default(),
    Visibility::Visible
  )
}
fn missile_visual(init_pos: Vec3, target: Entity) -> impl Bundle {
  (
    VisualEffect::MISSILE { init_pos, target },
    Visuals::material_sphere(MyMaterial::GLOWY_3),
    Transform::default(),
    Visibility::Visible
  )
}

#[derive(Component)]
struct OriginTime(u32);
fn origin_time(
  q: Query<Entity, Without<OriginTime>>,
  time_ticks: Res<TimeTicks>,
  mut c: Commands
) {
  for e in &q {
    c.entity(e).insert(OriginTime(time_ticks.0));
  }
}
fn combat_visual_effects(
  transformq: Query<&Transform, Without<VisualEffect>>,
  mut visualq: Query<(Entity, &mut Transform, &VisualEffect, &OriginTime)>,
  time_ticks: Res<TimeTicks>,
  mut c: Commands
) {
  for (entity, mut transform, visual_effect, origin_time) in &mut visualq {
    let visual_effect_age = time_ticks.0 - origin_time.0;
    match visual_effect.specify_transform(&transformq, visual_effect_age) {
      Some(new_transform) => *transform = new_transform,
      None => c.entity(entity).despawn_recursive()
    }
  }
}
#[derive(Component, Clone)]
pub struct IsHostile(pub bool);

enum CombatEffect {
  Damage(u32),
  Heal(u32)
}
enum Weapon {
  Laser,
  Blaster,
  MISSILE,
  RailGun,
  EMP,
  Ballistic,
  Drone
}
#[derive(Component, Default, Clone, Copy)]
pub struct Combat {
  pub hp: u32,
  pub shield: bool,
  pub is_hostile: bool,
  pub energy: u32
}
enum PlayerCombatAction {
  None,
  ShootNearest,
  ShootTargeted
}
#[derive(Clone, Copy)]
enum NonTargetedAbility {
  HealSelf(u32) // Drone
}
#[derive(Clone, Copy)]
enum TargetedAbility {
  FIRELaser(u32),
  FIREMISSILE(u32),
  HealOther(u32)
}
#[derive(Clone, Copy, Default)]
enum CombatAction {
  #[default]
  None,
  Targeted(TargetedAbility, Entity),
  NonTargeted(NonTargetedAbility)
}
const COMBAT_INTERVAL_TICKS: u32 = 380;
const COMBAT_RANGE: f32 = 150.0;
impl Combat {
  fn damage(&mut self, n: u32) { self.hp = self.hp.saturating_sub(n); }
  fn hp_depleted(&self) -> bool { self.hp == 0 }
  fn decrease_hp(&mut self, n: u32) { self.hp = self.hp.saturating_sub(n); }
  fn increase_hp(&mut self, n: u32) { self.hp += n; }
}
comment! {
  pub fn combat_system(mut c: Commands,
                       time: Res<TimeTicks>,
                       mut combat_query: Query<(Entity,
                                                &Transform,
                                                &mut Combat,
                                                Option<&IsHostile>
                       )>,
                       playerq: Single<(Entity, &Transform, &Player)>) {
    let modval = time.0 % COMBAT_INTERVAL_TICKS;
    if modval != 0 {
      if modval >= (COMBAT_INTERVAL_TICKS / 3) {
        for (entity, transform, mut combat) in &mut combat_query {
          if combat.hp_depleted() {
            c.spawn(explosion_visual(transform.translation, 2.0));
            c.entity(entity).despawn_recursive();
          }
        }
      }
      return;
    }

    let (player_entity, player_transform, player) = playerq.into_inner();

    let player_pos = player_transform.translation;
    let player_action = player.target()
                              .map_or(CombatAction::None, |target| {
                                CombatAction::Targeted(TargetedAbility::FIRELaser(10), target)
                              });
    let active_combatants: Vec<_> =
      combat_query.iter()
                  .filter_map(|(e, transform, _, _, InZone { in_player_zone })| {
                    in_player_zone.then_some((e, transform.translation))
                  })
                  .collect();
    let choose_action = |e: Entity, combat: Combat| {
      if e == player_entity {
        player_action
      } else if combat.is_hostile {
        if prob(0.5) {
          CombatAction::Targeted(TargetedAbility::FIRELaser(5), player_entity)
        } else {
          CombatAction::Targeted(TargetedAbility::FIREMISSILE(5), player_entity)
        }
      } else {
        CombatAction::None
      }
    };
    let combat_actions =
      vec(filter_map(|(e, transform, &combat, _, &InZone { in_player_zone })| {
        in_player_zone.then_some((e,
                                  transform.translation,
                                  choose_action(e, combat)))
      },
                     &combat_query));
    // map(|(self_entity, self_pos)| {
    //                          (self_entity, self_pos, choose_action(self_entity, self_pos))
    //                        },
    //                        active_combatants);
    for (self_entity, self_pos) in active_combatants {
      // println("aaaa");
      if let Ok((_, _, &self_combat, _, _)) = combat_query.get(self_entity) {
        let self_action = choose_action(self_entity, self_combat);
        match self_action {
          CombatAction::None => {}
          CombatAction::Targeted(targeted_ability, target) => {
            // println("cccc");
            if let Ok((_, _, mut target_combat, _, _)) = combat_query.get_mut(target) {
              match targeted_ability {
                TargetedAbility::FIRELaser(n) => {
                  // println("cccc");
                  target_combat.decrease_hp(n);
                  c.spawn(laser_visual(self_entity, target));
                }
                TargetedAbility::FIREMISSILE(n) => {
                  target_combat.decrease_hp(n);
                  c.spawn(missile_visual(self_pos, target));
                  // c.spawn(missile(self_pos, target, n));
                }
                TargetedAbility::HealOther(n) => {
                  target_combat.increase_hp(n);
                }
              }
            }
          }
          CombatAction::NonTargeted(non_targeted_ability) => {
            if let Ok((_, _, mut self_combat, _, _)) = combat_query.get_mut(self_entity) {
              match non_targeted_ability {
                NonTargetedAbility::HealSelf(n) => {
                  self_combat.increase_hp(n);
                }
              }
            }
          }
        }
      }
    }
  }
}
pub fn combat_system(
  mut c: Commands,
  time: Res<TimeTicks>,
  mut combat_query: Query<(Entity, &Transform, &mut Combat, Option<&IsHostile>)>,
  playerq: Single<(Entity, &Transform, &Player)>
) {
  // Define the proximity threshold for combat
  const PROXIMITY_THRESHOLD: f32 = 120.0; // Adjust this value as needed

  let modval = time.0 % COMBAT_INTERVAL_TICKS;
  if modval != 0 {
    if modval >= (COMBAT_INTERVAL_TICKS / 3) {
      for (entity, transform, mut combat, _) in &mut combat_query {
        if combat.hp_depleted() {
          c.spawn(explosion_visual(transform.translation, 2.0));
          c.entity(entity).despawn_recursive();
        }
      }
    }
    return;
  }

  let (player_entity, player_transform, player) = playerq.into_inner();
  let player_pos = player_transform.translation;
  let player_action = player.target().map_or(CombatAction::None, |target| {
    CombatAction::Targeted(TargetedAbility::FIRELaser(10), target)
  });

  // Collect active combatants within proximity threshold
  let active_combatants: Vec<_> = combat_query
    .iter()
    .filter_map(|(entity, transform, _, _)| {
      let distance = (transform.translation - player_pos).length();
      (distance <= PROXIMITY_THRESHOLD).then_some((entity, transform.translation))
    })
    .collect();

  let choose_action = |e: Entity, combat: &Combat| {
    if e == player_entity {
      player_action
    } else if combat.is_hostile {
      if prob(0.5) {
        CombatAction::Targeted(TargetedAbility::FIRELaser(5), player_entity)
      } else {
        CombatAction::Targeted(TargetedAbility::FIREMISSILE(5), player_entity)
      }
    } else {
      CombatAction::None
    }
  };

  for (self_entity, self_pos) in active_combatants {
    if let Ok((_, _, self_combat, _)) = combat_query.get(self_entity) {
      let self_action = choose_action(self_entity, self_combat);
      match self_action {
        CombatAction::None => {}
        CombatAction::Targeted(targeted_ability, target) => {
          if let Ok((_, _, mut target_combat, _)) = combat_query.get_mut(target) {
            match targeted_ability {
              TargetedAbility::FIRELaser(n) => {
                target_combat.decrease_hp(n);
                c.spawn(laser_visual(self_entity, target));
              }
              TargetedAbility::FIREMISSILE(n) => {
                target_combat.decrease_hp(n);
                c.spawn(missile_visual(self_pos, target));
                // c.spawn(missile(self_pos, target, n));
              }
              TargetedAbility::HealOther(n) => {
                target_combat.increase_hp(n);
              }
            }
          }
        }
        CombatAction::NonTargeted(non_targeted_ability) => {
          if let Ok((_, _, mut self_combat, _)) = combat_query.get_mut(self_entity) {
            match non_targeted_ability {
              NonTargetedAbility::HealSelf(n) => {
                self_combat.increase_hp(n);
              }
            }
          }
        }
      }
    }
  }
}

fn filter_least_map<O: Ord + Clone, T, R>(
  f: impl Fn(T) -> Option<(R, O)>,
  coll: impl IntoIterator<Item = T>
) -> Option<R> {
  coll.into_iter().filter_map(f).min_by_key(|(_, o)| o.clone()).map(|(r, _)| r)
}

fn filter_least<O: Ord + Clone, T>(
  f: impl Fn(&T) -> Option<O>,
  coll: impl IntoIterator<Item = T>
) -> Option<T> {
  filter_least_map(|t| f(&t).map(|v| (t, v)), coll)
}
fn filter_most_map<O: Ord + Clone, T, R>(
  f: impl Fn(T) -> Option<(R, O)>,
  coll: impl IntoIterator<Item = T>
) -> Option<R> {
  coll.into_iter().filter_map(f).max_by_key(|(_, o)| o.clone()).map(|(r, _)| r)
}
fn filter_most<O: Ord + Clone, T>(
  f: impl Fn(&T) -> Option<O>,
  coll: impl IntoIterator<Item = T>
) -> Option<T> {
  filter_most_map(|t| f(&t).map(|v| (t, v)), coll)
}
const ENEMY_SEE_PLAYER_RANGE: f32 = 100.0;
fn player_target_interaction(
  keys: Res<ButtonInput<KeyCode>>,
  mut playerq: Single<(Entity, &mut Player, &Transform, &mut Combat)>,
  mut hostileq: Query<(Entity, &IsHostile, &Transform)>,
  mut c: Commands,
  time: Res<TimeTicks>,
  targetq: Query<(&Transform,)>
) {
  let shoot_time_between = 60;
  let can_see_target = |e| true;
  let (
    player_entity,
    mut player,
    &player_transform @ Transform { translation: player_pos, .. },
    mut player_combat
  ) = playerq.into_inner();
  if keys.just_pressed(KeyCode::KeyQ) {
    player_combat.shield = !player_combat.shield;
  }
  if keys.just_pressed(KeyCode::KeyZ) {
    Object::MushroomMan.spawn_at(&mut c, player_pos);
  }
  if keys.just_pressed(KeyCode::KeyT) {
    todo!("fix this");
    println("pressed t");
    if let Some((e, _, _)) = filter_least(
      |(e, hostile, transform)| {
        hostile.0.then_some(transform.translation.distance(player_pos) as u32)
      },
      &hostileq
    ) {
      println("set target to {e}");
      player.set_target(e);
    }
  }
  if let Some(state) = player.target_interaction_state.as_mut()
    && let Ok(target_transform) = targetq.get(state.target)
    && can_see_target(state.target)
  {
    if keys.just_pressed(KeyCode::KeyR) {
      // shooting = !shooting;
      state.shooting = !state.shooting;
    }
    if keys.just_pressed(KeyCode::KeyQ) {
      state.approaching = !state.approaching;
    }
    if keys.just_pressed(KeyCode::KeyF) {
      c.spawn(missile_visual(player_pos, state.target));
    }
    if keys.just_pressed(KeyCode::KeyL) {
      c.spawn(laser_visual(player_entity, state.target));
    }
    if state.shooting && (time.0 % shoot_time_between == 0) {
      c.spawn(missile_visual(player_pos, state.target));
    }
    if keys.just_pressed(KeyCode::KeyX) {
      player.untarget();
    }
  } else {
    player.untarget();
  }
}

fn debug_input(
  keys: Res<ButtonInput<KeyCode>>,
  mut playerq: Single<(Entity, &mut Player, &Transform, &mut Combat)>,
  mut camq: Single<(Entity, &Transform, &Camera)>,
  mut hostileq: Query<(Entity, &IsHostile, &Transform)>,
  mut c: Commands,
  time: Res<TimeTicks>,
  targetq: Query<(&Transform,)>
) {
  let campos = camq.1.translation;
  if keys.just_pressed(KeyCode::KeyP) {
    dbg!(campos);
  }
}

#[derive(Component, Default, Clone)]
pub struct CONTAINER(pub HashSet<Entity>);
impl CONTAINER {
  pub fn empty() -> CONTAINER { CONTAINER::default() }
}
pub fn name(s: &'static str) -> Name { Name::new(s) }
#[derive(Component, Clone)]
pub struct TimedAnimation {
  pub num_frames: usize,
  pub time_per_frame_in_ticks: usize
}
#[derive(Component, Clone)]
pub struct PlayerFollower;
pub fn pick<T>(coll: impl IntoIterator<Item = T>) -> Option<T> {
  rand::seq::IteratorRandom::choose(coll.into_iter(), &mut thread_rng())
}
fn avg<T: std::iter::Sum + std::ops::Div<f32, Output = T>>(
  coll: impl IntoIterator<Item = T>
) -> Option<T> {
  let v = vec(coll);
  let n = v.len();
  let s = v.into_iter().sum::<T>();
  (n != 0).then(|| s / (n as f32))
}
pub fn capsule_from_height_and_radius(height: f32, radius: f32) -> Collider {
  Collider::capsule(height - (radius * 2.0), radius)
}
#[derive(Component, Clone, Default)]
pub struct SpaceObject {
  pub scale: f32,
  pub click_target_entity: Option<Entity>
}
#[derive(Component, Clone)]
pub struct ClickTarget;
pub fn click_target(
  mut parent_q: Query<&Parent>,
  mut click_events: EventReader<Pointer<Click>>,
  mut player: Single<&mut Player>
) {
  for event in click_events.read() {
    println(debugfmt(event));
    let mut root_entity = event.target;
    while let Ok(parent) = parent_q.get(root_entity) {
      root_entity = parent.get();
    }
    player.set_target(root_entity);
    println!("Player target set to {root_entity}");
  }
}
comment! {
fn spawn_at_pos(mut c: &mut Commands, b: impl Bundle, pos: Vec3) {
  c.spawn(b).insert(Transform::from_translation(pos)).observe(
    |trigger: Trigger<Pointer<Click>>, mut playerq: Single<&mut Player>| {
      playerq.set_target(trigger.entity());
    }
  );
}
  fn translated_space_object_with<B: Bundle + Clone>(
    scale: f32,
    can_move: bool,
    visuals: Visuals,
    b: B)
    -> impl Fn(Vec3) -> (B, SpaceObjectBundle) {
    move |translation| (b.clone(), space_object(translation, scale, can_move, visuals.clone()))
  }
}

pub fn set_space_object_scale(mut space_object_q: Query<(&mut Transform, &SpaceObject)>) {
  for (mut transform, space_object) in &mut space_object_q {
    transform.scale = Vec3::splat(space_object.scale);
  }
}

fn camera_follow_player(
  mut cam: Single<&mut PanOrbitCamera>,
  player_transform: Single<&Transform, With<Player>>
) {
  cam.target_focus = player_transform.translation;
}
#[derive(Default, Debug, Clone, Copy)]
enum NavigationKind {
  #[default]
  None,
  Dir3(Dir3),
  Vec3(Vec3),
  Pos(Vec3),
  Chase(Entity),
  ChaseAtRange(Entity, f32)
}
#[derive(Component, Debug, Clone, Copy, new)]
pub struct Navigation {
  // max_thrust: f32,
  max_speed: f32,
  #[new(default)]
  navigation_kind: NavigationKind
}
impl Navigation {
  fn speed(speed: f32) -> Self {
    Self { max_speed: speed, navigation_kind: NavigationKind::None }
  }
}

fn navigation(
  mut navigators_q: Query<(
    &Navigation,
    &Transform,
    &mut ExternalForce,
    &mut LinearVelocity
  )>,
  chase_targets_q: Query<&GlobalTransform>
) {
  for (
    &Navigation { max_speed, navigation_kind },
    &Transform { translation, .. },
    mut force,
    mut velocity
  ) in &mut navigators_q
  {
    let mut accelerate = |v: Vec3| {
      velocity.0 += v.normalize_or_zero() * 0.6;
      velocity.0 = velocity.0.clamp_length_max(max_speed);
    };
    match navigation_kind {
      NavigationKind::None => {}
      NavigationKind::Dir3(dir) => {
        accelerate(dir.as_vec3());
        // velocity.0 += ;
        // velocity.0 = velocity.0.clamp_length_max(max_speed);
        // force.apply_force(dir.as_vec3() * max_speed);
      }
      NavigationKind::Vec3(v) => {
        accelerate(v);
        // velocity.0 += v;
        // velocity.0 = velocity.0.clamp_length_max(max_speed);
        // force.apply_force(vec.normalize_or_zero() * max_speed);
      }
      NavigationKind::Pos(pos) => {
        let v = (pos - translation);
        accelerate(v);
        // velocity.0 += v;
        // velocity.0 = velocity.0.clamp_length_max(max_speed);
        // force.apply_force((pos - translation).normalize_or_zero() * max_speed);
      }
      NavigationKind::Chase(entity) => {
        if let Ok(entity_globaltransform) = chase_targets_q.get(entity) {
          let entity_pos = entity_globaltransform.translation();
          let rel = entity_pos - translation;
          force.apply_force(rel.normalize_or_zero() * max_speed);
        };
      }
      NavigationKind::ChaseAtRange(entity, range) => {
        if let Ok(entity_globaltransform) = chase_targets_q.get(entity) {
          let entity_pos = entity_globaltransform.translation();
          let rel = entity_pos - translation;
          let within_range = rel.length() < range;
          force.apply_force(
            rel.normalize_or_zero() * max_speed * if within_range { -1.0 } else { 1.0 }
          );
        };
      }
    }
  }
}
const PLAYER_FORCE: f32 = 170.0;
const PLAYER_SCALE: f32 = 1.2;
pub fn player_movement(
  keyboard_input: Res<ButtonInput<KeyCode>>,
  cam_transform: Single<&Transform, With<Camera3d>>,
  mut globaltransform_q: Query<&GlobalTransform>,
  mut playerq: Single<(
    Entity,
    &SpaceObject,
    &mut Navigation,
    &mut ExternalForce,
    &mut ExternalImpulse,
    &mut LinearVelocity,
    &Transform,
    &mut Player
  )>
) {
  let (
    player_entity,
    player_space_object,
    mut player_navigation,
    mut player_force,
    mut player_impulse,
    mut player_vel,
    player_transform,
    mut player
  ) = playerq.into_inner();
  let up = Vec3::Y;
  let forward = Vec3 { y: 0.0, ..cam_transform.forward().into() }.normalize_or_zero();
  let right = forward.cross(up);

  let Vec3 { x, y, z } =
    sum(filter_map(|(key, v)| keyboard_input.pressed(key).then_some(v), [
      (KeyCode::KeyA, Vec3::NEG_X),
      (KeyCode::KeyS, Vec3::NEG_Z),
      (KeyCode::KeyD, Vec3::X),
      (KeyCode::KeyW, Vec3::Z),
      (KeyCode::ControlLeft, Vec3::NEG_Y),
      (KeyCode::ShiftLeft, Vec3::Y)
    ]))
    .normalize_or_zero();
  let keyb_dir = (x * right) + (z * forward) + (y * up);
  player_navigation.navigation_kind =
    if let Some(PlayerTargetInteractionState { target, approaching, .. }) =
      player.target_interaction_state
      && approaching
    {
      NavigationKind::Chase(target)
    } else {
      NavigationKind::Vec3(keyb_dir)
    };
}
comment! {
  #[derive(Bundle)]
  struct SceneSpaceObjectBundle((Handle<Scene>, SpaceObjectBundle));
  impl SceneSpaceObjectBundle {
    fn new(translation: Vec3, scale: f32, can_move: bool, scene: Handle<Scene>) -> Self {
      Self((scene, space_object(scale, can_move, Visuals::sprite(MySprite::COFFEE))))
    }
  }
}
// pub const RAPIER_CONFIG: RapierConfiguration =
//   RapierConfiguration { gravity: Vec3::ZERO,
//                         physics_pipeline_active: true,li
//                         query_pipeline_active: true,
//                         timestep_mode: todo!(),
//                         scaled_shape_subdivision: todo!(),
//                         force_update_from_transform_changes: todo!() };

pub fn warp(
  mut player_transform: Single<&mut Transform, With<Player>>,
  targetq: Populated<&GlobalTransform, With<WarpGate>>,
  keyboard_input: Res<ButtonInput<KeyCode>>
) {
  if keyboard_input.just_pressed(KeyCode::KeyG) {
    let target_globaltransform = pick(targetq.iter()).unwrap();
    player_transform.translation = target_globaltransform.translation();
  }
}
#[derive(Default, Resource)]
pub struct TimeTicks(pub u32);
pub fn increment_time(mut time: ResMut<TimeTicks>) { time.0 += 1; }
comment! {
  pub fn timed_animation_system(time_ticks: Res<TimeTicks>,
                                mut q: Query<(&TimedAnimation, &mut TextureAtlas)>) {
    for (&TimedAnimation { num_frames,
                           time_per_frame_in_ticks },
         mut atlas) in &mut q
    {
      let time = time_ticks.0 as usize;
      let index = |time| (time / time_per_frame_in_ticks) % num_frames;
      let old_index = index(time.saturating_sub(1));
      let new_index = index(time);
      if new_index != old_index {
        atlas.index = new_index;
      }
    }
  }
}

fn close_on_esc(mut exit: EventWriter<AppExit>, keyboard_input: Res<ButtonInput<KeyCode>>) {
  if keyboard_input.just_pressed(KeyCode::Escape) {
    exit.send(AppExit::Success);
  }
}

// #[derive(Resource, Default)]
// pub enum GameState {
//   #[default]
//   FlyingInSpace,
//   WarpGui(ui::WarpGui)
// }
// #[derive(Event, Clone, Copy)]
// pub enum GuiInputEvent {
//   Up,
//   Down,
//   Left,
//   Right,
//   Select,
//   Cancel
// }
// convert this enum to a wrapper struct with named fields around &'static str and CharacterAlignment. convert variants to const instances in an impl block
// #[derive(Eq, PartialEq, Clone, Copy, Assoc, Default, Debug)]
// #[func(pub const fn alignment(&self) -> CharacterAlignment)]
// pub enum Faction {
//   #[default]
//   #[assoc(alignment = CharacterAlignment::Neutral)]
//   Wanderers,
//   #[assoc(alignment = CharacterAlignment::LawfulGood)]
//   SpacePolice,
//   #[assoc(alignment = CharacterAlignment::ChaoticEvil)]
//   SpacePirates,
//   #[assoc(alignment = CharacterAlignment::ChaoticNeutral)]
//   SPACEWIZARDs,
//   #[assoc(alignment = CharacterAlignment::Neutral)]
//   Traders,
//   #[assoc(alignment = CharacterAlignment::LawfulEvil)]
//   Invaders,
//   #[assoc(alignment = CharacterAlignment::NeutralGood)]
//   Explorers,
//   #[assoc(alignment = CharacterAlignment::Neutral)]
//   Neutral,
// }

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
enum CharacterAlignment {
  LawfulGood,
  LawfulNeutral,
  LawfulEvil,
  NeutralGood,
  Neutral,
  NeutralEvil,
  ChaoticGood,
  ChaoticNeutral,
  ChaoticEvil
}
#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub struct Faction {
  pub name: &'static str,
  pub alignment: CharacterAlignment
}
// finish writing these to use the new function
impl Faction {
  pub const fn new(name: &'static str, alignment: CharacterAlignment) -> Self {
    Self { name, alignment }
  }
  pub const fn alignment(&self) -> CharacterAlignment { self.alignment }

  pub const WANDERERS: Self = Self::new("Wanderers", CharacterAlignment::Neutral);
  pub const SPACE_POLICE: Self = Self::new("SpacePolice", CharacterAlignment::LawfulGood);
  pub const SPACE_PIRATES: Self = Self::new("SpacePirates", CharacterAlignment::ChaoticEvil);
  pub const SPACEWIZARDS: Self =
    Self::new("SPACEWIZARDs", CharacterAlignment::ChaoticNeutral);
  pub const TRADERS: Self = Self::new("Traders", CharacterAlignment::Neutral);
  pub const INVADERS: Self = Self::new("Invaders", CharacterAlignment::LawfulEvil);
  pub const EXPLORERS: Self = Self::new("Explorers", CharacterAlignment::NeutralGood);
  pub const NEUTRAL: Self = Self::new("Neutral", CharacterAlignment::Neutral);
}
impl Default for Faction {
  fn default() -> Self { Self::NEUTRAL }
}
#[derive(Component, Clone)]
struct MagicAbility;

struct NPCProps {
  faction: Faction,
  name: Option<&'static str>,
  base_hp: u32,
  thrust: f32,
  sprite: MySprite,
  magic_ability: Option<MagicAbility>
}
impl Faction {
  fn is_good(&self) -> bool {
    matches!(
      self.alignment(),
      CharacterAlignment::LawfulGood
        | CharacterAlignment::NeutralGood
        | CharacterAlignment::ChaoticGood
    )
  }
  fn is_bad(&self) -> bool { !self.is_good() }
  fn is_hostile(&self, target: Self) -> bool {
    (self.is_bad() || target.is_bad()) && (*self != target)
  }
}
fn colorful_texture() -> Image {
  let texture_size = 8;
  Image::new_fill(
    bevy::render::render_resource::Extent3d {
      width: texture_size,
      height: texture_size,
      depth_or_array_layers: 1
    },
    bevy::render::render_resource::TextureDimension::D2,
    map(|_| rand::random(), 0..((texture_size * texture_size * 4) as usize))
      .collect::<Vec<u8>>()
      .as_slice(),
    bevy::render::render_resource::TextureFormat::Rgba8UnormSrgb,
    bevy::render::render_asset::RenderAssetUsages::RENDER_WORLD
  )
}

#[derive(Component, Clone)]
pub struct Enemy;

type DialogueEffect = fn() -> MyCommand;
type DialogueTreeNode = (
  &'static str,
  &'static [(&'static str, &'static str, &'static str, Option<DialogueEffect>)]
);
const DIALOGUE_END: DialogueTreeNode = ("END", &[]);
type DialogueTree = &'static [DialogueTreeNode];

pub const SPHERICAL_SPACE_COW_DIALOGUE: DialogueTree = &[
  ("A", &[("B", "Hello there, cow!", "Cow: \"Moo-stronaut reporting for duty!\"", None)]),
  ("B", &[
    (
      "C",
      "Why are you in space?",
      "Cow: \"I'm here to study zero-gravity milk production!\"",
      None
    ),
    (
      "D",
      "How did you become spherical?",
      "Cow: \"It's an evolutionary adaptation for space travel.\"",
      None
    ),
    ("E", "Are you lost?", "Cow: \"No, space is my new pasture!\"", None)
  ]),
  ("C", &[
    (
      "F",
      "How's the milk production going?",
      "Cow: \"It's out of this world! Want to try some Moon Moo?\"",
      None
    ),
    (
      "G",
      "Isn't it dangerous up here?",
      "Cow: \"No need to cow-er in fear, I've got my space suit!\"",
      None
    ),
    (
      "H",
      "Who sent you here?",
      "Cow: \"Dr. Bovine von Lactose, the mad dairy scientist!\"",
      None
    )
  ]),
  ("D", &[
    (
      "I",
      "What are the advantages of being spherical?",
      "Cow: \"I can roll in any direction, and there are no corners to bump into!\"",
      None
    ),
    (
      "J",
      "Are there other spherical animals in space?",
      "Cow: \"I've heard rumors of a cubical chicken, but that's just absurd.\"",
      None
    ),
    (
      "K",
      "Can you change back to normal?",
      "Cow: \"Why would I? Being spherical is utterly amazing!\"",
      None
    )
  ]),
  ("E", &[
    (
      "L",
      "Don't you miss Earth?",
      "Cow: \"Sometimes, but the view up here is spe-cow-tacular!\"",
      None
    ),
    (
      "M",
      "What do you eat in space?",
      "Cow: \"Cosmic ray grass and star dust. It's quite moo-tritious!\"",
      None
    ),
    (
      "N",
      "How do you moo-ve around?",
      "Cow: \"I just roll with it! Newton's laws are my best friends.\"",
      None
    )
  ]),
  ("F", &[(
    "O",
    "Yes, I'd love to try some!",
    "Cow: \"Here's a glass of Moon Moo. It's extra frothy in zero-G!\"",
    None
  )]),
  ("G", &[(
    "P",
    "What's the biggest danger you've faced?",
    "Cow: \"I once got caught in Saturn's rings. Talk about a tight spot!\"",
    None
  )]),
  ("H", &[(
    "Q",
    "Can I meet this scientist?",
    "Cow: \"He's on the dark side of the moon. It's a bit of a trek!\"",
    None
  )]),
  ("I", &[(
    "R",
    "Can you demonstrate your rolling?",
    "Cow: \"Sure! WATch me do a barrel roll!\"",
    None
  )]),
  ("J", &[(
    "S",
    "A cubical chicken? That's crazy!",
    "Cow: \"I know, right? Geometry in space gets wild!\"",
    None
  )]),
  ("K", &[(
    "T",
    "Do you ever get dizzy from being round?",
    "Cow: \"Nope, I'm always well-balanced!\"",
    None
  )]),
  ("L", &[(
    "U",
    "What's your favorite view from space?",
    "Cow: \"The Milky Way, of course! It reminds me of home.\"",
    None
  )]),
  ("M", &[(
    "V",
    "Does star dust taste good?",
    "Cow: \"It's a bit dry, but it makes my milk sparkle!\"",
    None
  )]),
  ("N", &[(
    "W",
    "Can you explain the physics of your movement?",
    "Cow: \"It's all about conservation of moo-mentum!\"",
    None
  )]),
  ("O", &[(
    "X",
    "Wow, it's delicious! Can I have the recipe?",
    "Cow: \"Sorry, it's a closely guarded secret of the cosmos.\"",
    None
  )]),
  ("P", &[(
    "Y",
    "That sounds terrifying! How did you escape?",
    "Cow: \"I used my quick re-flex-es and dairy-ing escape plan!\"",
    None
  )]),
  ("Q", &[(
    "Z",
    "Is he planning to send more animals to space?",
    "Cow: \"He's working on a flock of zero-gravity sheep as we speak!\"",
    None
  )]),
  ("R", &[(
    "AA",
    "Impressive! Do you ever get motion sick?",
    "Cow: \"Nah, I've got a stomach of steel... er, four of them actually!\"",
    None
  )]),
  ("S", &[(
    "AB",
    "Are there any other strange space animals?",
    "Cow: \"I've heard whispers of a dodecahedron dolphin, but that's just silly.\"",
    None
  )]),
  ("T", &[(
    "AC",
    "You're full of jokes! Are all space cows this funny?",
    "Cow: \"Of course! Humor helps us cope with the uni-verse-al challenges.\"",
    None
  )]),
  ("U", &[(
    "AD",
    "That's beautiful. Do you ever feel lonely up here?",
    "Cow: \"Sometimes, but then I remember I'm surrounded by stars... and star-struck fans like you!\"",
    None
  )]),
  ("V", &[(
    "AE",
    "Sparkly milk sounds amazing! Can it grant superpowers?",
    "Cow: \"Only the power of good bone density and a happy tummy!\"",
    None
  )]),
  ("W", &[(
    "AF",
    "You're quite the physicist! Ever thought of teaching?",
    "Cow: \"I've been thinking of starting a 'Moo-niversity' actually!\"",
    None
  )]),
  ("X", &[(
    "END",
    "I understand. Thanks for sharing it with me!",
    "Cow: \"You're welcome! Remember, what happens in space, stays in space!\"",
    None
  )]),
  ("Y", &[(
    "END",
    "You're quite the adventurer! Any other close calls?",
    "Cow: \"Well, there was this one time with a black hole... but that's a story for another day!\"",
    None
  )]),
  ("Z", &[(
    "END",
    "Wow! What's next, pigs in orbit?",
    "Cow: \"Don't be silly! Everyone knows pigs can't fly... yet.\"",
    None
  )]),
  ("AA", &[(
    "END",
    "You're amazing! Can I take a selfie with you?",
    "Cow: \"Of course! Let's make it a 'span-selfie' - spanning the cosmos!\"",
    None
  )]),
  ("AB", &[(
    "END",
    "This is getting too weird. I think I need to go.",
    "Cow: \"Aw, don't have a cow, man! Stay a while and listen to more space tales!\"",
    None
  )]),
  ("AC", &[(
    "END",
    "You're out of this world! Thanks for the chat!",
    "Cow: \"My pleasure! Remember, in space, everyone can hear you cream... your coffee!\"",
    None
  )]),
  ("AD", &[(
    "END",
    "You're never alone with that attitude! Goodbye, space cow!",
    "Cow: \"Goodbye, Earth friend! May your dreams be as boundless as the universe!\"",
    None
  )]),
  ("AE", &[(
    "END",
    "I'll take a gallon! This was fun, thanks!",
    "Cow: \"Come back anytime! The Milky Way's always open!\"",
    None
  )]),
  ("AF", &[(
    "END",
    "SIGN me up for Astro-nomoo-my 101! Farewell!",
    "Cow: \"So long, and thanks for all the laughs! Keep reaching for the stars!\"",
    None
  )]),
  DIALOGUE_END
];

pub const SPACE_COWBOY_DIALOGUE: DialogueTree = &[
  ("A", &[(
    "B",
    "Howdy, partner!",
    "Space Cowboy: \"Well, howdy there, space traveler! Welcome to the cosmic corral!\"",
    None
  )]),
  ("B", &[
    (
      "C",
      "What's a cowboy doing in space?",
      "Space Cowboy: \"Roundin' up asteroids and headin' off comet stampedes, of course!\"",
      None
    ),
    (
      "D",
      "Nice space suit! Is that leather?",
      "Space Cowboy: \"Sure is! Genuine Martian leather, tougher than a solar flare!\"",
      None
    ),
    (
      "E",
      "Have you seen any aliens?",
      "Space Cowboy: \"Aliens? Why, I've shared a campfire with beings from more galaxies than you can count!\"",
      None
    )
  ]),
  ("C", &[
    (
      "F",
      "ASTEROID roundup? How does that work?",
      "Space Cowboy: \"With a quantum lasso and a whole lotta patience, partner!\"",
      None
    ),
    (
      "G",
      "Comet stampedes sound dangerous!",
      "Space Cowboy: \"You bet your stars they are! But nothin' my trusty rocket horse can't handle.\"",
      None
    )
  ]),
  ("D", &[
    (
      "H",
      "Martian leather? Is that ethical?",
      "Space Cowboy: \"Now, don't you worry. It's all synthetic, made from Mars dust. No space cows harmed!\"",
      None
    ),
    (
      "I",
      "How does it protect you from space?",
      "Space Cowboy: \"It's lined with nanotech fibers. Keeps out cosmic rays better than a fort keeps out rustlers!\"",
      None
    )
  ]),
  ("E", &[
    (
      "J",
      "Tell me about these aliens!",
      "Space Cowboy: \"Met a cloud being from Nebula Nine once. Makes a mean vapor coffee!\"",
      None
    ),
    (
      "K",
      "A cosmic campfire? How?",
      "Space Cowboy: \"With a contained plasma flame, 'course! Roasts space marshmallows like you wouldn't believe.\"",
      None
    )
  ]),
  ("F", &[(
    "END",
    "That sounds amazing! Can you teach me?",
    "Space Cowboy: \"Sure thing, greenhorn! First lesson: always approach an asteroid from downwind.\"",
    None
  )]),
  ("G", &[(
    "END",
    "A rocket horse? Now I've heard everything!",
    "Space Cowboy: \"Ol' Supernova here's been my loyal steed for light-years! Ain't ya, girl?\" *pats invisible horse*",
    None
  )]),
  ("H", &[(
    "END",
    "That's a relief! It looks so realistic.",
    "Space Cowboy: \"Yep, fools even the keenest eye. Now, if you'll excuse me, I've got some solar wind to wrangle!\"",
    None
  )]),
  ("I", &[(
    "END",
    "Incredible! Where can I get one?",
    "Space Cowboy: \"These suits are rarer than a quiet night in a neutron star saloon. But if you prove yourself, I might know a fella...\"",
    None
  )]),
  ("J", &[(
    "END",
    "Vapor coffee? That's wild!",
    "Space Cowboy: \"Puts hair on your chest and a twinkle in your eye! Now, if you'll pardon me, I've got a date with the Milky Way.\"",
    None
  )]),
  ("K", &[(
    "END",
    "Space marshmallows? Now I'm hungry!",
    "Space Cowboy: \"Tell ya what, next time you're in the Andromeda arm, look me up. We'll have ourselves a good ol' space hoedown!\"",
    None
  )]),
  DIALOGUE_END
];

pub const SOCRATES_DIALOGUE: DialogueTree = &[
  ("A", &[(
    "B",
    "Greetings, Socrates! How are you finding space?",
    "Socrates: \"Ah, greetings, young seeker of knowledge! Space, like wisdom, is vast and full of wonder.\"",
    None
  )]),
  ("B", &[
    (
      "C",
      "What do you think about this future world?",
      "Socrates: \"I know that I know nothing of this world, which makes it all the more fascinating to question and explore.\"",
      None
    ),
    (
      "D",
      "Can you tell me about your philosophical method?",
      "Socrates: \"Even here, amidst the stars, we must question everything. Shall we examine the nature of this cosmic realm?\"",
      None
    ),
    (
      "E",
      "How does space travel relate to your ideas of the soul?",
      "Socrates: \"Perhaps our souls, like these celestial bodies, are on an eternal journey through the universe of ideas.\"",
      None
    )
  ]),
  ("C", &[
    (
      "F",
      "Does this advanced technology change your views on knowledge?",
      "Socrates: \"Technology may advance, but the pursuit of wisdom remains unchanged. We must still question and reflect.\"",
      None
    ),
    (
      "G",
      "What would you ask the aliens if we meet them?",
      "Socrates: \"I would ask them about their concept of virtue, and whether it's universal across the cosmos.\"",
      None
    )
  ]),
  ("D", &[
    (
      "H",
      "How would you apply the Socratic method to space exploration?",
      "Socrates: \"We must question our assumptions about the universe, just as we question our beliefs about ourselves.\"",
      None
    ),
    (
      "I",
      "Can your ideas of ethics apply to alien civilizations?",
      "Socrates: \"The search for universal truths should extend beyond Earth. Perhaps aliens too seek the good life.\"",
      None
    )
  ]),
  ("E", &[
    (
      "J",
      "Do you think space travel could be a form of seeking truth?",
      "Socrates: \"Indeed! As we journey through space, are we not also journeying through the realm of ideas?\"",
      None
    ),
    (
      "K",
      "How does floating in space compare to your concept of the Forms?",
      "Socrates: \"This weightlessness reminds me of how the soul must feel when contemplating the Forms. Utterly free!\"",
      None
    )
  ]),
  ("F", &[(
    "END",
    "That's profound. Thank you for your wisdom, Socrates.",
    "Socrates: \"Remember, the unexamined space life is not worth living! Now, shall we ponder the ethics of faster-than-light travel?\"",
    None
  )]),
  ("G", &[(
    "END",
    "Alien virtue? That's a mind-bending concept!",
    "Socrates: \"Indeed! And in questioning them, we may learn more about ourselves. Now, I wonder if there's a cosmic equivalent of hemlock...\"",
    None
  )]),
  ("H", &[(
    "END",
    "I see. Question everything, even in space!",
    "Socrates: \"Precisely! Now, let us question the very nature of these asteroid fields. What is their essence?\"",
    None
  )]),
  ("I", &[(
    "END",
    "Universal ethics across species... fascinating!",
    "Socrates: \"A worthy pursuit indeed! Now, if you'll excuse me, I must go contemplate the allegory of the black hole.\"",
    None
  )]),
  ("J", &[(
    "END",
    "Space travel as a metaphor for seeking truth. Brilliant!",
    "Socrates: \"You show wisdom, young space traveler. Now, shall we examine the true form of these twinkling stars?\"",
    None
  )]),
  ("K", &[(
    "END",
    "Your ideas truly transcend time and space, Socrates.",
    "Socrates: \"As do all ideas, my friend. Now, I must float away and dialectically analyze this cosmic dust.\"",
    None
  )]),
  DIALOGUE_END
];

pub const MARIE_CURIE_DIALOGUE: DialogueTree = &[
  ("A", &[(
    "B",
    "Madame Curie! It's an honor. How are you adapting to space?",
    "Marie Curie: \"Bonjour! The universe is full of natural marvels. I'm detecting fascinating radiation patterns!\"",
    None
  )]),
  ("B", &[
    (
      "C",
      "What do you think about modern space technology?",
      "Marie Curie: \"C'est incroyable! The advances in physics and chemistry have led to marvels beyond my wildest dreams.\"",
      None
    ),
    (
      "D",
      "How does your work on radioactivity apply here?",
      "Marie Curie: \"The principles remain the same, but the scale is enormous! Cosmic rays, solar radiation... so much to study!\"",
      None
    ),
    (
      "E",
      "What would you like to research in space?",
      "Marie Curie: \"I'm fascinated by the potential for new elements in these asteroids. Shall we start collecting samples?\"",
      None
    )
  ]),
  ("C", &[
    (
      "F",
      "Do you think space travel would have changed your research?",
      "Marie Curie: \"Undoubtedly! The absence of gravity opens up new possibilities for experiments in radioactivity.\"",
      None
    ),
    (
      "G",
      "What advice would you give to future scientists?",
      "Marie Curie: \"Never fear the unknown. In science and in space, curiosity is our greatest asset.\"",
      None
    )
  ]),
  ("D", &[
    (
      "H",
      "How would you protect astronauts from cosmic radiation?",
      "Marie Curie: \"We must study it first! Understanding radiation is key to protection. Perhaps a new element could help...\"",
      None
    ),
    (
      "I",
      "Could your work on X-rays be applied to space medicine?",
      "Marie Curie: \"Absolutely! Imagine a portable X-ray device for diagnosing injuries on long space voyages.\"",
      None
    )
  ]),
  ("E", &[
    (
      "J",
      "What kind of lab equipment would you need for space research?",
      "Marie Curie: \"A spectrometer would be essential. And perhaps we could design a microgravity centrifuge for separation!\"",
      None
    ),
    (
      "K",
      "Do you think we might find radioactive alien life?",
      "Marie Curie: \"An intriguing hypothesis! We must approach it with rigorous scientific method and an open mind.\"",
      None
    )
  ]),
  ("F", &[(
    "END",
    "Your passion for science is truly inspiring, Madame Curie.",
    "Marie Curie: \"Merci! Remember, in science as in space exploration, we must have perseverance and faith in the unknown.\"",
    None
  )]),
  ("G", &[(
    "END",
    "Great advice! Science and exploration go hand in hand.",
    "Marie Curie: \"Indeed! Now, shall we analyze the spectral lines of that nearby star? For science!\"",
    None
  )]),
  ("H", &[(
    "END",
    "A new element for radiation shielding? Brilliant idea!",
    "Marie Curie: \"Every discovery opens new doors. Now, let's calibrate this space-suited Geiger counter!\"",
    None
  )]),
  ("I", &[(
    "END",
    "Space X-rays... that could revolutionize long-distance space travel!",
    "Marie Curie: \"Exactement! Science knows no borders, not even in the vastness of space. Now, where did I put my radium samples...\"",
    None
  )]),
  ("J", &[(
    "END",
    "A space lab sounds amazing. You're already adapting to the future!",
    "Marie Curie: \"Science evolves, but the spirit of inquiry remains. Now, let's see what secrets these cosmic rays hold!\"",
    None
  )]),
  ("K", &[(
    "END",
    "Radioactive aliens? Now that's a sci-fi concept!",
    "Marie Curie: \"Science often surpasses fiction! Now, help me set up this zero-gravity polonium experiment, s'il vous plaît.\"",
    None
  )]),
  DIALOGUE_END
];

pub const ABRAHAM_LINCOLN_DIALOGUE: DialogueTree = &[
  ("A", &[(
    "B",
    "President Lincoln! How are you finding the space age?",
    "Lincoln: \"Four score and seven light-years ago... I jest. This future is both terrifying and awe-inspiring.\"",
    None
  )]),
  ("B", &[
    (
      "C",
      "How do your ideas of democracy apply to space colonization?",
      "Lincoln: \"A government of the planets, by the planets, for the planets, shall not perish from this universe.\"",
      None
    ),
    (
      "D",
      "What do you think about the current state of equality?",
      "Lincoln: \"Progress has been made, but our journey continues. We must ensure liberty and justice for all sentient beings.\"",
      None
    ),
    (
      "E",
      "How would you handle diplomacy with alien races?",
      "Lincoln: \"With malice toward none, with charity for all... even those with tentacles or exoskeletons.\"",
      None
    )
  ]),
  ("C", &[
    (
      "F",
      "Should every planet have equal representation?",
      "Lincoln: \"A house divided against itself cannot stand, even if that house spans galaxies. We must find a way to unite.\"",
      None
    ),
    (
      "G",
      "What about AI rights in this futuristic society?",
      "Lincoln: \"The notion that all intelligences are created equal must extend to artificial ones too. It's the next frontier of rights.\"",
      None
    )
  ]),
  ("D", &[
    (
      "H",
      "Have we achieved your vision of equality?",
      "Lincoln: \"Progress is evident, but the work is never finished. We must strive to extend equality across the cosmos.\"",
      None
    ),
    (
      "I",
      "How can we apply your principles to alien civilizations?",
      "Lincoln: \"The better angels of our nature must guide us in treating all sentient life with respect and dignity.\"",
      None
    )
  ]),
  ("E", &[
    (
      "J",
      "Would you still believe in preserving the Union on a galactic scale?",
      "Lincoln: \"The principles remain sound. We must work to form a more perfect Union, even among the stars.\"",
      None
    ),
    (
      "K",
      "How would you address conflicts between human colonies and alien worlds?",
      "Lincoln: \"Let us strive on to finish the work we are in, to achieve and cherish a just and lasting peace among ourselves and all sentient beings.\"",
      None
    )
  ]),
  ("F", &[(
    "END",
    "A galactic democracy... that's a big idea, Mr. President!",
    "Lincoln: \"Indeed it is! Now, if you'll excuse me, I need to draft the Emancipation Proclamation for the robots of Neptune...\"",
    None
  )]),
  ("G", &[(
    "END",
    "AI rights? You're adapting quickly to future issues!",
    "Lincoln: \"The principles of liberty are timeless, my friend. Now, shall we discuss the ethics of faster-than-light travel?\"",
    None
  )]),
  ("H", &[(
    "END",
    "Your vision continues to inspire us, even in space.",
    "Lincoln: \"Remember, the struggle for equality is as vast as space itself. Now, I must contemplate the Gettysburg Address for Martians.\"",
    None
  )]),
  ("I", &[(
    "END",
    "Respecting all sentient life... a noble goal for the future.",
    "Lincoln: \"Indeed. The task remaining before us is as great as the cosmos itself. Now, where can a man get a stovepipe helmet for his spacesuit?\"",
    None
  )]),
  ("J", &[(
    "END",
    "A galactic Union... that's an incredible concept!",
    "Lincoln: \"The work of unity never ceases, my friend. Now, I believe I have a speech to give at the Andromeda Lincoln Memorial.\"",
    None
  )]),
  ("K", &[(
    "END",
    "Your words of peace resonate even in the space age, sir.",
    "Lincoln: \"May they echo across the stars. Now, I must attend to pressing matters. I hear there's a vampire problem on the dark side of the moon...\"",
    None
  )]),
  DIALOGUE_END
];

pub const CHRONOS_SPACE_WIZARD_DIALOGUE: DialogueTree = &[
  ("A", &[(
    "B",
    "Who are you, and why have you brought these historical figures to space?",
    "Space Wizard: \"Greetings, cosmic traveler! I am Chronos, the Space Wizard of Time and Dimension. I have assembled these great minds for a grand purpose!\"",
    None
  )]),
  ("B", &[
    (
      "C",
      "What is this grand purpose?",
      "Chronos: \"To solve the greatest challenges of the universe! These brilliant minds, combined with futuristic knowledge, might save reality itself!\"",
      None
    ),
    (
      "D",
      "How did you bring them here?",
      "Chronos: \"With my Chrono-Spatial Translocator, of course! It plucks beings from their timestreams and deposits them here, fully adapted to space travel.\"",
      None
    ),
    (
      "E",
      "Won't this disrupt the timeline?",
      "Chronos: \"Fear not! Once our task is complete, I shall return them to their exact moments in history, memories intact but disguised as vivid dreams.\"",
      None
    )
  ]),
  ("C", &[
    (
      "F",
      "What are these universal challenges?",
      "Chronos: \"The heat death of the universe, the reconciliation of quantum mechanics and general relativity, and the correct way to eat a cosmic sandwich in zero gravity!\"",
      None
    ),
    (
      "G",
      "How can historical figures help with future problems?",
      "Chronos: \"Fresh perspectives, my friend! Sometimes the wisdom of the past is key to unlocking the mysteries of the future.\"",
      None
    )
  ]),
  ("D", &[
    (
      "H",
      "Is the Chrono-Spatial Translocator safe?",
      "Chronos: \"Mostly! There was that one incident with Cleopatra and the black hole, but we don't talk about that...\"",
      None
    ),
    (
      "I",
      "Can anyone use this device?",
      "Chronos: \"Goodness, no! It requires a degree in Temporal Physics and a license from the Intergalactic Time Authority. Plus, really good spatial awareness.\"",
      None
    )
  ]),
  ("E", &[
    (
      "J",
      "What if they want to stay in the future?",
      "Chronos: \"An excellent question! But history must run its course. Their contributions in their own times are crucial to the development of humanity.\"",
      None
    ),
    (
      "K",
      "Could their future knowledge change history?",
      "Chronos: \"Their memories of this adventure will fade upon return, leaving only subconscious inspiration. Clever, eh?\"",
      None
    )
  ]),
  ("F", &[(
    "END",
    "Those are... interesting challenges. Especially the sandwich one.",
    "Chronos: \"Never underestimate the importance of proper space cuisine! Now, excuse me while I explain quantum entanglement to Socrates.\"",
    None
  )]),
  ("G", &[(
    "END",
    "I see. It's like a cosmic think tank!",
    "Chronos: \"Precisely! Now, if you'll pardon me, I need to stop Marie Curie from trying to split atoms on the ship.\"",
    None
  )]),
  ("H", &[(
    "END",
    "Mostly safe? That's... reassuring.",
    "Chronos: \"Don't worry! The chances of accidental dinosaur materialization are very low this time. Now, where did I put that temporal stabilizer...\"",
    None
  )]),
  ("I", &[(
    "END",
    "I see. So no borrowing it for weekend trips to the Renaissance.",
    "Chronos: \"I'm afraid not. Last time someone did that, we ended up with pizza in ancient Egypt. Now, I must calibrate the quantum flux capacitor!\"",
    None
  )]),
  ("J", &[(
    "END",
    "That makes sense. It's a big responsibility.",
    "Chronos: \"Indeed it is! The burden of knowledge is heavy, but the fate of the cosmos is heavier. Now, I need to explain internet memes to Abe Lincoln.\"",
    None
  )]),
  ("K", &[(
    "END",
    "Subconscious inspiration... very clever indeed!",
    "Chronos: \"Thank you! Now, if you'll excuse me, I need to prevent Nikola Tesla from rewiring our ship's power grid. Again.\"",
    None
  )]),
  DIALOGUE_END
];
pub const LEONARDO_DA_VINCI_DIALOGUE: DialogueTree = &[
  ("A", &[(
    "B",
    "Leonardo da Vinci! How are you finding the future?",
    "Leonardo: \"Ah, the marvels of this age! My mind overflows with new inventions and artworks inspired by the cosmos!\"",
    None
  )]),
  ("B", &[
    (
      "C",
      "What do you think of modern technology?",
      "Leonardo: \"Magnifico! Though I must say, I had already envisioned many of these contraptions. See this spacesuit? I'm improving its design as we speak!\"",
      None
    ),
    (
      "D",
      "How does space travel compare to your flying machine concepts?",
      "Leonardo: \"It's beyond my wildest dreams! Yet, the principles of flight I studied apply even here. Observe how we maneuver through this asteroid field!\"",
      None
    ),
    (
      "E",
      "Would you like to paint this cosmic scenery?",
      "Leonardo: \"Oh, if only I had my easel! The play of light on these celestial bodies... it's the ultimate study of chiaroscuro!\"",
      None
    )
  ]),
  ("C", &[
    (
      "F",
      "What improvements would you make to our technology?",
      "Leonardo: \"I've been sketching designs for more efficient solar sails and a da Vinci-style space station. Care to take a look?\"",
      None
    ),
    (
      "G",
      "How does this era inspire your creativity?",
      "Leonardo: \"The blend of art and science here is exquisite! I'm particularly intrigued by your holographic displays. An art form in itself!\"",
      None
    )
  ]),
  ("D", &[
    (
      "H",
      "Could your studies on bird flight help with space maneuvering?",
      "Leonardo: \"Indubitably! The grace of a bird and the dance of a spacecraft are not so different. It's all about understanding flow and resistance.\"",
      None
    ),
    (
      "I",
      "What do you think of modern aviation?",
      "Leonardo: \"It's a dream realized! Though I must say, these rockets seem a bit inelegant. Perhaps we could design something more... artistic?\"",
      None
    )
  ]),
  ("E", &[
    (
      "J",
      "How would you capture the essence of space in art?",
      "Leonardo: \"I would blend the mathematical precision of star charts with the fluid beauty of nebulae. A fusion of the scientific and the divine!\"",
      None
    ),
    (
      "K",
      "Would you be interested in creating art with our future tools?",
      "Leonardo: \"Absolutely! Imagine the possibilities of sculpting with zero-gravity 3D printers or painting with light itself!\"",
      None
    )
  ]),
  ("F", &[(
    "END",
    "Your ideas could revolutionize space travel, even now!",
    "Leonardo: \"Grazie mille! Now, if you'll excuse me, I must discuss the golden ratio with that charming nebula over there.\"",
    None
  )]),
  ("G", &[(
    "END",
    "Your excitement for blending art and science is contagious!",
    "Leonardo: \"Art, science, technology - they are all one in the pursuit of knowledge and beauty! Now, where did I leave my anti-gravity sketchbook?\"",
    None
  )]),
  ("H", &[(
    "END",
    "Birds and spaceships... I never thought of it that way!",
    "Leonardo: \"Nature is the greatest teacher, even among the stars! Now, I must continue my studies on the aerodynamics of space debris.\"",
    None
  )]),
  ("I", &[(
    "END",
    "An artistic rocket? That's an intriguing concept!",
    "Leonardo: \"Form and function in perfect harmony! Now, let me show you my preliminary sketches for a Vitruvian Spaceman...\"",
    None
  )]),
  ("J", &[(
    "END",
    "Your cosmic art sounds breathtaking. I can't wait to see it!",
    "Leonardo: \"The universe itself is the ultimate masterpiece! Now, if you'll pardon me, I need to recalibrate the golden ratio for non-Euclidean space.\"",
    None
  )]),
  ("K", &[(
    "END",
    "Sculpting in zero-g... Now that would be something to see!",
    "Leonardo: \"Indeed! The possibilities are as endless as space itself. Now, I must go - I have an appointment to exchange ideas with a sentient gas cloud!\"",
    None
  )]),
  DIALOGUE_END
];

pub const CLEOPATRA_DIALOGUE: DialogueTree = &[
  ("A", &[(
    "B",
    "Queen Cleopatra! How are you adapting to the space age?",
    "Cleopatra: \"Greetings, cosmic traveler. I must say, ruling a galactic empire would have been... intriguing.\"",
    None
  )]),
  ("B", &[
    (
      "C",
      "How does space travel compare to sailing the Nile?",
      "Cleopatra: \"The Nile was but a stream compared to this river of stars. Though I do miss the crocodiles... perhaps we could find some space equivalents?\"",
      None
    ),
    (
      "D",
      "What do you think about modern politics and diplomacy?",
      "Cleopatra: \"Politics, like the cosmos, is vast and complex. But whether on Earth or among the stars, alliances and strategy remain key.\"",
      None
    ),
    (
      "E",
      "How would you apply your leadership skills in this era?",
      "Cleopatra: \"An empire among the stars... now that's an ambition worthy of a pharaoh! I would unite planets as I united Egypt and Rome.\"",
      None
    )
  ]),
  ("C", &[
    (
      "F",
      "Space crocodiles? That's an interesting idea!",
      "Cleopatra: \"Indeed! Every queen needs her guardians. Besides, I'm sure there are plenty of cosmic treasures to protect in this vast universe.\"",
      None
    ),
    (
      "G",
      "What aspects of space exploration fascinate you most?",
      "Cleopatra: \"The diversity of worlds reminds me of the cultures along the Mediterranean. Each unique, yet connected by the cosmic seas.\"",
      None
    )
  ]),
  ("D", &[
    (
      "H",
      "How would you handle diplomacy with alien races?",
      "Cleopatra: \"With grace, wisdom, and a hint of mystery. Whether dealing with Romans or Reptilians, a grand entrance is essential.\"",
      None
    ),
    (
      "I",
      "What lessons from your era apply to galactic politics?",
      "Cleopatra: \"Power is about perception and alliances. Even in space, one must know when to be the asp and when to be the charm.\"",
      None
    )
  ]),
  ("E", &[
    (
      "J",
      "A galactic empire? That's quite ambitious!",
      "Cleopatra: \"Go big or go home, as they say. Though in space, I suppose everywhere is home. First, we'll need a cosmic Alexandria...\"",
      None
    ),
    (
      "K",
      "How would you structure a government across planets?",
      "Cleopatra: \"A pharaoh for each world, united under a galactic regent. Myself, naturally. With faster-than-light communication, governance should be a breeze.\"",
      None
    )
  ]),
  ("F", &[(
    "END",
    "I'll keep an eye out for space crocodiles, Your Majesty.",
    "Cleopatra: \"Do that, dear friend. Now, if you'll excuse me, I must review the blueprints for my orbital pyramid.\"",
    None
  )]),
  ("G", &[(
    "END",
    "Your insight draws beautiful parallels, Your Highness.",
    "Cleopatra: \"Thank you. The universe, like Egypt, is full of hidden treasures. Now, I'm off to negotiate mining rights with the asteroid belt pharaohs.\"",
    None
  )]),
  ("H", &[(
    "END",
    "Diplomacy through mystery and grandeur. Classic Cleopatra!",
    "Cleopatra: \"One must keep the mystique alive, even in a spacesuit. Now, be a dear and help me plan my zero-gravity barge procession.\"",
    None
  )]),
  ("I", &[(
    "END",
    "The asp and the charm... a timeless strategy, it seems.",
    "Cleopatra: \"In politics, some things never change. Now, I must go charm the Arcturian ambassador. Or was it the Betelgeusian regent?\"",
    None
  )]),
  ("J", &[(
    "END",
    "A cosmic Alexandria sounds magnificent!",
    "Cleopatra: \"Doesn't it? With a library containing the knowledge of a million worlds! Now, if you'll excuse me, I need to discuss funding with the Galactic Senate.\"",
    None
  )]),
  ("K", &[(
    "END",
    "Your administrative skills are truly universal, Your Highness.",
    "Cleopatra: \"Naturally. Now, I must go. These star charts won't decipher themselves, and I have a galaxy to unite!\"",
    None
  )]),
  DIALOGUE_END
];
const NORMAL_NPC_SCALE: f32 = 1.9;
const NORMAL_NPC_SPEED: f32 = 400.0;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Item(pub &'static str);

impl Item {
  pub const fn new(name: &'static str) -> Self { Self(name) }
  pub const SPACECAT: Self = Self::new("space cat");
  pub const PERSON: Self = Self::new("person");
  pub const SPICE: Self = Self::new("spice");
  pub const COFFEE: Self = Self::new("coffee");
  pub const SPACECOIN: Self = Self::new("spacecoin");
  pub const CRYSTAL: Self = Self::new("crystal");
  pub const DIHYDROGENMONOXIDE: Self = Self::new("dihydrogen monoxide");
  pub const ROCK: Self = Self::new("rock");
  pub const SPACEMINERALS: Self = Self::new("space minerals");
}

#[derive(Component, Clone, Debug, Default)]
pub struct Inventory(HashMap<Item, u32>);

impl Inventory {
  fn add_contents(&mut self, contents: impl IntoIterator<Item = (Item, u32)>) {
    for (item, n) in contents {
      *(self.0.entry(item).or_default()) += n;
    }
  }
  fn trade(
    &mut self,
    inputs: impl IntoIterator<Item = (Item, u32)>,
    outputs: impl IntoIterator<Item = (Item, u32)>
  ) {
    for (item, n) in outputs {
      *(self.0.entry(item).or_default()) += n;
    }
    for (item, n) in inputs {
      *(self.0.entry(item).or_default()) -= n;
    }
  }
  fn from_contents(contents: impl IntoIterator<Item = (Item, u32)>) -> Self {
    Self(contents.into_iter().collect())
  }
}

pub fn ndm(n: u32, m: u32) -> u32 {
  let mut rng = rand::thread_rng();
  (0..n).map(|_| rng.gen_range(1..=m)).sum()
}
pub fn nd6(n: u32) -> u32 { ndm(n, 6) }
pub fn nd20(n: u32) -> u32 { ndm(n, 20) }
pub fn one_d6() -> u32 { nd6(1) }
pub fn two_d6() -> u32 { nd6(2) }
pub fn one_d20() -> u32 { nd20(1) }
// Helper that wraps a closure into an Interact::MultipleOptions.
// fn minigame(
//   f: impl FnMut(
//     &mut World
//   ) -> (String, Vec<(String, MyCommand, Box<dyn MultipleOptionsInteractFn>)>)
//   + 'static
// ) -> Interact {
//   Interact::MultipleOptions(InteractMultipleOptions::new(f))
// }

comment! {
struct OptionSet {
  options: Vec<(String, Box<dyn FnOnce()>)>
}
impl OptionSet {
  fn new() -> Self { Self { options: default() } }
  fn add<F>(mut self, label: impl ToString, f: F) -> Self
  where
    F: FnOnce()
  {
    self.options.push((label.to_string(), Box::new(f)));
    self
  }

  fn add_if<F>(self, condition: bool, label: impl ToString, f: F) -> Self
  where
    F: FnOnce()
  {
    if condition { self.add(label, f) } else { self }
  }
}
  fn self_mutating_closure() -> Box<dyn FnMut(&mut World) -> (String, OptionSet)> {
    let mut resource_quantity = 0;
    let mut durability = 5;
    let mut has_dynamite = true;
    Box::new(|world| {
      let msg = "theres's a rock here that you can mine".to_string();
      let options = OptionSet::new()
        .add_if(has_dynamite, "throw dynamite", || {
          has_dynamite = false;
          resource_quantity += 2;
        })
        .add_if(durability > 0, "dig", || {
          durability -= 1;
          resource_quantity += 1;
        })
        .add("leave", || {
          world.give_items_to_player([(Item::SPACEMINERALS, resource_quantity)]);
          resource_quantity = 0;
        });
      (msg, options)
    })
  }
}
// Helper to create an option with a short closure that produces the next state.
// This simply converts the provided closure (with no world dependency) into the boxed closure.
// fn mk_option(
//   text: &str,
//   command: MyCommand,
//   mut next: impl FnMut()
//     -> (String, Vec<(String, MyCommand, Box<dyn MultipleOptionsInteractFn>)>)
//   + 'static
// ) -> (String, MyCommand, Box<dyn MultipleOptionsInteractFn>) {
//   (
//     text.to_string(),
//     command,
//     Box::new(move |_world| next()) as Box<dyn MultipleOptionsInteractFn>
//   )
// }

// // A super–concise asteroid-mining minigame. The local mutable state is captured in the closure,
// // and the entire Interact::MultipleOptions value is constructed in one go with minigame() and inline options.
// fn simple_mining_minigame(mut resources: u8, mut durability: u8) -> Interact {
//   minigame(move |_world| {})
// }

#[derive(Clone)]
enum InteractMultipleOptions {
  Salvage { how_much_loot: u8 },
  WarpGate { name: &'static str },
  DialogueTree(DialogueTree, &'static str),
  AsteroidMiningMiniGame { resources_left: u8, tool_durability: u8 }
}
impl InteractMultipleOptions {
  fn interact(self) -> (String, Vec<(String, MyCommand, Self)>) {
    match self {
      InteractMultipleOptions::AsteroidMiningMiniGame {
        resources_left,
        tool_durability
      } => {
        let msg = format!(
          "You're mining an asteroid. Resources left: {}. Tool durability: {}.",
          resources_left, tool_durability
        );
        let mut options = vec![];

        if resources_left > 0 && tool_durability > 0 {
          options.push((
            "Mine carefully".to_string(),
            MyCommand::multi([
              MyCommand::message_add("You mine carefully, preserving your tool."),
              MyCommand::give_item_to_player(Item::SPACEMINERALS)
            ]),
            Self::AsteroidMiningMiniGame {
              resources_left: resources_left - 1,
              tool_durability
            }
          ));
          options.push((
            "Mine aggressively".to_string(),
            MyCommand::multi([
              MyCommand::message_add(
                "You mine aggressively, risking your tool for more resources."
              ),
              MyCommand::give_item_to_player(Item::SPACEMINERALS),
              MyCommand::give_item_to_player(Item::SPACEMINERALS)
            ]),
            Self::AsteroidMiningMiniGame {
              resources_left: resources_left - 1,
              tool_durability: tool_durability - 1
            }
          ));
        }

        options.push((
          "Leave asteroid".to_string(),
          MyCommand::end_object_interaction_mini_game(),
          self.clone()
        ));

        (msg, options)
      }
      InteractMultipleOptions::Salvage { how_much_loot } => {
        let msg = "It's a destroyed spaceship. Maybe you can find loot in it".to_string();
        let options = if how_much_loot > 0 {
          vec![
            (
              "take some".to_string(),
              MyCommand::multi([
                MyCommand::message_add("You found loot"),
                MyCommand::give_item_to_player(Item::SPACECOIN)
              ]),
              Self::Salvage { how_much_loot: how_much_loot - 1 }
            ),
            ("don't take".to_string(), MyCommand::none(), self.clone()),
            (
              "leave".to_string(),
              MyCommand::end_object_interaction_mini_game(),
              self.clone()
            ),
          ]
        } else {
          vec![(
            "leave".to_string(),
            MyCommand::end_object_interaction_mini_game(),
            self.clone()
          )]
        };
        (msg, options)
      }
      InteractMultipleOptions::DialogueTree(tree, node) => {
        let msg = "talking npc".to_string();
        if let Some((_, options)) = tree.iter().find(|(node2, options)| *node2 == node) {
          let options = options.iter().map(|(new_node, playersay, npcsay, effect)| {
            (
              playersay.to_string(),
              MyCommand::message_add(npcsay.to_string()),
              InteractMultipleOptions::DialogueTree(tree, *new_node)
            )
          });
          (msg, options.collect())
        } else {
          (msg, default())
        }
      }
      // todo!
      InteractMultipleOptions::WarpGate { name } => {
        ("this is a warp gate".to_string(), vec![(
          "...".to_string(),
          MyCommand::message_add("WIP"),
          InteractMultipleOptions::WarpGate { name }
        )])
      }
    }
  }
}
// #[derive(Component)]
// struct NewInteract(Box<dyn std::fmt::Debug>);

#[derive(Clone)]
enum InteractSingleOption {
  Message(String),
  // Salvage { how_much_loot: u8 },
  ASTEROID,
  HPBOX,
  Describe,
  Item(Item),
  Trade { inputs: (Item, u32), outputs: (Item, u32) },
  GATE(Vec3),
  CONTAINER(Vec<(Item, u32)>)
}

impl InteractSingleOption {
  fn interact(
    self,
    self_entity: Entity,
    self_name: String,
    player_inventory: &Inventory
  ) -> (String, MyCommand) {
    match self {
      InteractSingleOption::Message(m) => ("examine".to_string(), MyCommand::message_add(m)),
      InteractSingleOption::ASTEROID => {
        (format!("examine {self_name}"), MyCommand::message_add("it's an asteroid"))
      }
      InteractSingleOption::HPBOX => (
        "take hp box".to_string(),
        MyCommand::multi([
          MyCommand::update_player_component(|combat: Combat| Combat {
            hp: combat.hp + 50,
            ..combat
          }),
          MyCommand::despawn(self_entity)
        ])
      ),
      InteractSingleOption::Describe => {
        (format!("examine {self_name}"), MyCommand::message_add(self_name))
      }
      InteractSingleOption::Item(item) => (
        format!("take {self_name}"),
        MyCommand::multi([
          MyCommand::despawn(self_entity),
          MyCommand::message_add(format!("You got a {}", debugfmt(item))),
          MyCommand::give_item_to_player(item)
        ])
      ),

      InteractSingleOption::Trade {
        inputs: (input_item, input_number),
        outputs: (output_item, output_number)
      } => (
        "trade".to_string(),
        if let Some(&n) = player_inventory.0.get(&input_item)
          && n >= input_number
        {
          MyCommand::multi([
            MyCommand::mutate_player_component(move |mut inventory: &mut Inventory| {
              inventory.trade([(input_item, input_number)], [(output_item, output_number)]);
            }),
            MyCommand::message_add(format!(
              "You traded {:?} {:?} for {:?} {:?}s",
              input_number, input_item, output_number, output_item
            ))
          ])
        } else {
          MyCommand::message_add("You don't have enough items")
        }
      ),
      InteractSingleOption::GATE(destination_pos) => (
        "interact".to_string(),
        MyCommand::update_player_component(move |transform| Transform {
          translation: destination_pos,
          ..transform
        })
      ),
      InteractSingleOption::CONTAINER(items) => (
        "take container".to_string(),
        MyCommand::multi([
          MyCommand::despawn(self_entity),
          MyCommand::message_add("you got things"),
          MyCommand::mutate_player_component(|mut inventory: &mut Inventory| {
            inventory.add_contents(items);
          })
        ])
      )
    }
  }
}

#[derive(Component, Clone)]
enum Interact {
  SingleOption(InteractSingleOption),
  MultipleOptions(InteractMultipleOptions)
}

impl Interact {
  fn dialogue_tree_default_state(tree: DialogueTree) -> Self {
    let (node, _) = tree[0];
    Self::MultipleOptions(InteractMultipleOptions::DialogueTree(tree, node))
  }
}
const INTERACTION_RANGE: f32 = 8.0;
fn interact(
  mut playerq: Single<(Entity, &mut Transform, &Combat, &Inventory), With<Player>>,
  mut interactable_q: Query<
    (Entity, &Transform, &mut Interact, Option<&Name>),
    Without<Player>
  >,
  gate_q: Query<(&GlobalTransform, &WarpGate)>,
  mut c: Commands,
  keys: Res<ButtonInput<KeyCode>>,
  mut ui_data: ResMut<UIData>
) {
  // ui_data.interact_message = None;
  let (player, player_transform, player_combat, player_inventory) = playerq.into_inner();
  let player_pos = player_transform.translation;
  let closest_interactable_thing = filter_least(
    |tup| {
      let dist = tup.1.translation.distance(player_pos);
      (dist < INTERACTION_RANGE).then_some(dist as u32)
    },
    &mut interactable_q
  );
  if let Some((interact_entity, transform, mut interact, oname)) = closest_interactable_thing
  {
    match interact.as_mut() {
      Interact::SingleOption(interact_single_option) => {
        let (message, command) = interact_single_option.clone().interact(
          interact_entity,
          namefmt(oname),
          player_inventory
        );
        // ui_data.interact_message = Some(format!("[SPACE: {message}]"));

        INTERACT_MESSAGE.set(Some(format!("[SPACE: {message}]")));
        if keys.just_pressed(KeyCode::Space) {
          c.queue(command);
        }
      }
      Interact::MultipleOptions(interact_multiple_options) => {
        let (msg, options) = interact_multiple_options.clone().interact();
        INTERACT_MESSAGE.set(Some(intersperse_newline([msg, default()].into_iter().chain(
          (&options).into_iter().enumerate().map(|(n, tup)| format!("{}: {}", n + 1, tup.0))
        ))));
        let number_picked =
          find_map(|(n, key): (u8, KeyCode)| keys.just_pressed(key).then_some(n), [
            (0, KeyCode::Digit0),
            (1u8, KeyCode::Digit1),
            (2, KeyCode::Digit2),
            (3, KeyCode::Digit3),
            (4, KeyCode::Digit4),
            (5, KeyCode::Digit5),
            (6, KeyCode::Digit6),
            (7, KeyCode::Digit7),
            (8, KeyCode::Digit8),
            (9, KeyCode::Digit9)
          ]);
        for (n, (string, command, new_interact)) in options.into_iter().enumerate() {
          if number_picked == Some(n as u8 + 1) {
            c.queue(command);
            *interact_multiple_options = new_interact.clone();
          }
        }
      }
    }
  } else {
    INTERACT_MESSAGE.set(None);
  }
}

pub const MESSAGE_LOG_MAX_LEN: usize = 5;
const MESSAGE_SHOW_TIME_TICKS: u32 = 300;

#[derive(Clone, Debug)]
pub struct Message {
  pub time: u32,
  pub string: String
}

impl From<Message> for String {
  fn from(msg: Message) -> Self { msg.string }
}
impl From<String> for Message {
  fn from(string: String) -> Self { Message { time: MESSAGE_SHOW_TIME_TICKS, string } }
}

const UI_BACKGROUND_COLOR: Color = Color::srgba(0.0, 0.0, 0.0, 0.5);
const UI_BORDER_COLOR: Color = Color::srgba(0.0, 0.0, 0.0, 0.7);
const UI_TEXT_COLOR: Color = Color::WHITE;
const UI_FONT_SIZE: f32 = 22.0; // Adjusted font size slightly
const UI_PADDING: Val = Val::Px(5.0);
const UI_BORDER: Val = Val::Px(4.0);

static MESSAGE_LOG: Lazy<Mutable<Vec<Message>>> = Lazy::new(default);
static OVERVIEW_DATA: Lazy<Mutable<Vec<String>>> = Lazy::new(default);
static INFOBOX_DATA: Lazy<Mutable<Vec<String>>> = Lazy::new(default);
static TARGET_DATA: Lazy<Mutable<Vec<String>>> = Lazy::new(default);
static INTERACT_MESSAGE: Lazy<Mutable<Option<String>>> = Lazy::new(default);

comment! {
  pub fn common_style(sb: &mut StyleBuilder) {
    sb.font_size(32.0)
      .display(Display::Block)
      .border(3)
      .border_color(UI_BORDER_COLOR)
      .background_color(UI_BACKGROUND_COLOR)
      .position(bevy::ui::PositionType::Absolute)
      .color(UI_FONT_COLOR)
      .pointer_events(false);
  }
}
fn ui_box<
  T: Into<String>,
  C: IntoIterator<Item = T> + 'static,
  S: Signal<Item = C> + Send + 'static
>(
  align: Align,
  signal: S,
  font_handle: Handle<Font>
) -> impl Element {
  Column::<Text>::new()
    .background_color(BackgroundColor(UI_BACKGROUND_COLOR))
    .border_color(BorderColor(UI_BORDER_COLOR))
    .border_radius(BorderRadius::all(UI_BORDER))
    .align(Some(align))
    .width(Val::Auto)
    .height(Val::Auto)
    .text_signal(signal.map(|c: C| Text(intersperse_newline(c))))
}
// fn infobox(font_handle: Handle<Font>) -> impl Element {
//   ui_box(Align::new().left().top(), INFOBOX_DATA.signal_cloned(), font_handle)
// }

fn root_ui(font_handle: Handle<Font>) -> impl Element {
  Stack::<NodeBundle>::new() // Stack remains suitable for layering
    .width(Val::Percent(100.0))
    .height(Val::Percent(100.0))
    // Layer the panels, using the simplified/generic functions
    // message log
    .layer(ui_box(
      Align::new().left().bottom(),
      MESSAGE_LOG.signal_cloned(),
      font_handle.clone()
    ))
    // infobox
    .layer(ui_box(
      Align::new().left().top(),
      INFOBOX_DATA.signal_cloned(),
      font_handle.clone()
    ))
    // overview
    .layer(ui_box(
      Align::new().right().top(),
      OVERVIEW_DATA.signal_cloned(),
      font_handle.clone()
    ))
    // target box
    .layer(ui_box(
      Align::new().right().bottom(),
      TARGET_DATA.signal_cloned(),
      font_handle.clone()
    ))
    // interact message box
    .layer(ui_box(
      Align::new().center_x().center_y(),
      INTERACT_MESSAGE.signal_cloned(),
      font_handle.clone()
    ))
}

#[derive(Resource, Default, Clone)]
pub struct UIData {
  // target info...
  pub current_time_ticks: u32,
  pub message_log: Vec<Message>,
  pub overview_data: Vec<String>,
  // pub player_pos: Vec3,
  pub count: u32,
  pub foo: usize,
  pub font: Handle<Font>,
  pub interact_message: Option<String>,
  pub target_data: Vec<String>,
  pub infobox_data: Vec<String> // pub target_interaction_state: Option<PlayerTargetInteractionState>,
                                // pub space_cat_count: u32,
                                // pub player_inventory: Inventory
}
pub fn intersperse_newline<T: Into<String>>(coll: impl IntoIterator<Item = T>) -> String {
  concat_strings(coll.into_iter().map(|v| v.into()).intersperse("\n".to_string()))
}

impl UIData {
  pub fn message_add(&mut self, message: impl ToString) {
    let time = self.current_time_ticks;
    self.message_log.push(Message { string: message.to_string().into(), time });
  }
}

fn setup_ui_system(world: &mut World) {
  let asset_server = world.resource::<AssetServer>();
  // <<< IMPORTANT: Make sure this path is correct for your project >>>
  let font_handle: Handle<Font> = default();
  root_ui(font_handle).spawn(world); // Spawn the UI root
  // Add an initial message if desired
  MESSAGE_LOG.lock_mut().push(Message::from("Welcome!".to_string()));
}

fn namefmt(oname: Option<&Name>) -> String {
  match oname {
    Some(name) => name.to_string(),
    None => "unnamed entity".to_string()
  }
}
fn signal_strength(player_pos: Vec3, pos: Vec3, scale: f32) -> f32 {
  scale.powi(2) / (player_pos.distance(pos)).powi(2)
}

// please finish rewriting the various local variables in fn ui by using TargetData and a lot of method chains and filter_map and such
#[derive(QueryData)]
struct TargetData {
  entity: Entity,
  transform: &'static Transform,
  spaceobject: &'static SpaceObject,
  oname: Option<&'static Name>,
  ocombat: Option<&'static Combat>,
  oplanet: Option<&'static Planet>
}

pub fn ui(
  playerq: Query<(Entity, &Player, &Transform, &Combat, &Inventory)>, // Changed to Query
  target_q: Query<TargetData>,
  // mut ui_data: ResMut<UIData>,
  time: Res<TimeTicks>
) {
  // Use get_single for clarity, handling potential errors/no player
  let Ok((_player_entity, player, player_transform, player_combat, player_inventory)) =
    playerq.get_single()
  else {
    // Handle case where player doesn't exist yet or multiple exist
    // *ui_data = UIData::default(); // Reset UI or show an error state
    return;
  };
  let player_pos = player_transform.translation;

  // --- Infobox Data ---
  let infobox_data: Vec<String> = [
    format!("Pos: {:.1}", player_pos),
    format!("HP: {}", player_combat.hp),
    format!("Energy: {}", player_combat.energy),
    "Controls:".to_string(),
    " w,a,s,d,shift,ctrl: move".to_string(),
    " z: spawn mushroom man".to_string(),
    " q: toggle shield".to_string(),
    " t: target nearest hostile".to_string(),
    " g: warp".to_string(),
    " l: shoot laser".to_string(),
    " r: toggle shoot".to_string(),
    " x: untarget".to_string(),
    "Inventory:".to_string()
  ]
  .into_iter()
  .chain(player_inventory.0.iter().map(|(item, n)| format!(" - {}x {:?}", n, item)))
  .collect();
  INFOBOX_DATA.set(infobox_data);

  // --- Target Data ---
  let target_data: Vec<String> = player
    .target()
    .and_then(|target_entity| target_q.get(target_entity).ok())
    .map(|target| {
      let distance = player_pos.distance(target.transform.translation);
      let name = target.oname.cloned().unwrap_or(Name::new("unknown"));
      [
        Some(format!("Target: {}", name)),
        target.oplanet.map(|p| format!("Planet Info: {:?}", p)), // Uses Display impl
        target.ocombat.map(|c| format!("HP: {}", c.hp)),
        Some(format!("Distance: {:.1}", distance)) // Add context-dependent actions if needed
                                                   // e.g., if target.oplanet.is_some() { Some("F: Land".to_string()) } else { None }
      ]
      .into_iter()
      .flatten() // Remove None options
      .collect()
    })
    .unwrap_or_default(); // Use empty Vec if no target or target data not found
  TARGET_DATA.set(target_data);

  // --- Overview Data (Hostiles in Combat Range) ---
  let player_dist = |t: &TargetDataItem| player_pos.distance(t.transform.translation);
  let targetqvec: Vec<TargetDataItem> = vec(&target_q);
  let overview_data: Vec<String> = targetqvec
    .into_iter()
    // .sort_by_key(|i: &TargetDataItem| player_pos.distance(i.transform.translation) as i32)
    // .sort_by(|a, b| {
    //   player_dist(*a).partial_cmp(&player_dist(**b)).unwrap_or(std::cmp::Ordering::Equal)
    // })
    .filter_map(|target| {
      // Calculate distance first
      // Filter for hostiles within range that have combat stats and a name
      let distance = player_dist(&target);
      let name = target.oname.map_or("object", Name::as_str);
      (distance < COMBAT_RANGE).then_some({
        if let Some(combat) = target.ocombat {
          let hostilestr = if combat.is_hostile { "!!" } else { "" };
          format!(
            "{}{}{} (HP:{}) <-> {:.1}",
            hostilestr, name, hostilestr, combat.hp, distance
          )
        } else {
          format!("{} <-> {:.1}", name, distance)
        }
      })
    })
    // .take(12)
    .collect();
  OVERVIEW_DATA.set(overview_data);

  MESSAGE_LOG.update_mut(|v| {
    *v = v
      .clone()
      .into_iter()
      .filter_map(|Message { time, string }| {
        (time > 0).then(|| Message { time: time - 1, string })
      })
      .collect();
  });
}

pub fn string(t: impl ToString) -> String { t.to_string() }
#[derive(Component, Clone, Default)]
struct CanBeFollowedByNPC;
#[derive(Component, Clone, Default, new)]
struct NPC {
  #[new(default)]
  pub follow_target: Option<Entity>,
  pub faction: Faction
}
const NPC_FORCE: f32 = 420.0;
const NPC_FOLLOW_RANGE_MAX: f32 = 300.0;
const NPC_FOLLOW_RANGE_MIN: f32 = 10.0;
fn npc_movement(
  mut npc_q: Query<(&mut NPC, &mut Navigation, &GlobalTransform)>,
  follow_target_q: Query<(Entity, &GlobalTransform), With<CanBeFollowedByNPC>>
) {
  // TextureAtlas
  // Image
  let get_target_pos = |e| -> Option<Vec3> {
    follow_target_q.get(e).ok().map(|(_, globaltransform)| globaltransform.translation())
  };
  for (mut npc, mut npc_navigation, npc_globaltransform) in &mut npc_q {
    let npc_pos = npc_globaltransform.translation();
    // let pos_in_range = |pos: Vec3| {
    //   let dist = pos.distance(npc_pos);
    //   dist > NPC_FOLLOW_RANGE_MIN && dist < NPC_FOLLOW_RANGE_MAX
    // };
    let in_range = |e: Entity| {
      get_target_pos(e).map_or(false, |pos: Vec3| {
        let dist = pos.distance(npc_pos);
        dist > NPC_FOLLOW_RANGE_MIN && dist < NPC_FOLLOW_RANGE_MAX
      })
    };
    if let Some(target_entity) = npc.follow_target
      && in_range(target_entity)
    {
      npc_navigation.navigation_kind = NavigationKind::ChaseAtRange(target_entity, 4.0);
    } else {
      npc.follow_target =
        pick(filter_map(|(e, _)| in_range(e).then_some(e), &follow_target_q));
    }
  }
}

#[derive(Clone, Component, Debug)]
struct Planet {
  // pub planet_type: PlanetType,
  pub population: u32
}
pub struct NamedNPC {
  name: &'static str,
  faction: Faction,
  sprite: MySprite,
  dialogue_tree: DialogueTree
}

enum Degree {
  Lower,
  Low,
  Med,
  High,
  Higher
}

#[derive(Clone, Copy, Debug)]
pub struct PlanetType {
  pub sprite: MySprite
}
impl PlanetType {
  pub const fn new(sprite: MySprite) -> Self { Self { sprite } }
  pub const fn sprite(&self) -> MySprite { self.sprite }
  pub const MARSLIKEPLANET: Self = Self::new(MySprite::MARSLIKEPLANET);
  pub const HABITABLEPLANET: Self = Self::new(MySprite::HABITABLEPLANET);
  pub const SANDPLANET: Self = Self::new(MySprite::DESERT_PLANET_IMAGEN_3);
  pub const ICEPLANET: Self = Self::new(MySprite::ICEPLANET);
  pub const LAVAPLANET: Self = Self::new(MySprite::LAVAPLANET);
  pub const BROWNGASGIANT: Self = Self::new(MySprite::BROWNGASGIANT);
}

#[derive(Clone, Copy, Debug)]
enum Object {
  Empty,
  SpaceObject {
    // SpaceObjects have a sphere shaped collider where the scale is the radius. good to keep in mind when spawning large objects
    name: &'static str,
    scale: f32,
    can_move: bool,
    visuals: Visuals
  },
  NPC {
    name: &'static str,
    hp: u32,
    speed: f32,
    sprite: MySprite,
    scale: f32,
    faction: Faction
  },
  Enemy {
    name: &'static str,
    hp: u32,
    speed: f32,
    sprite: MySprite
  },
  LootObject {
    sprite: MySprite,
    scale: f32,
    name: &'static str,
    item_type: Item
  },
  Explorer,
  ScaledNPC {
    scale: f32,
    name: &'static str,
    speed: f32,
    faction: Faction,
    hp: u32,
    sprite: MySprite
  },
  // can be talked to but cannot move. A moving, talking npc would be hard to interact with
  TalkingPerson {
    name: &'static str,
    sprite: MySprite,
    dialogue_tree: DialogueTree
  },
  Sun,
  Planet {
    sprite: MySprite,
    radius: f32,
    population: u32,
    name: &'static str
  },
  WarpGate {
    name: &'static str
  },
  AbandonedShip,
  AlienSoldier,
  Asteroid,
  CrystalAsteroid,
  CrystalMonster,
  CrystalMonster2,
  FloatingIsland,
  HpBox,
  IceAsteroid,
  Miner,
  MinerNpc,
  MushroomMan,
  Nomad,
  Player,
  Sign {
    text: &'static str
  },
  SpaceCat,
  SpaceCoin,
  SpaceCop,
  SpaceCowboy,
  SpacePirate,
  SpacePirateBase,
  SpaceStation,
  SpaceWizard,
  Spaceman,
  SphericalCow,
  TradeStation,
  Trader,
  TreasureContainer,
  Wormhole //  BlackHole,
           //  SpaceJellyfish,
           //  WormholePortal,
           //  SpaceWhale,
           //  AlienArtifact,
           //  SPACESTATION,
           //  QuantumAnomaly,
           //  SpaceMine,
           //  NebulaCloud,
           //  SolarFlare,
           //  SpaceDebris,
           //  CometCluster,
           //  DysonSPHERE,
           //  AncientRuins,
           //  AlienOutpost,
           //  RepairDrone,
           //  SalvageYard,
           //  FuelDepot,
           //  SpaceCasino,
           //  ResearchLab,
           //  CloakedShip,
           //  SpaceBarnacle,
           //  CosmicSpore,
           //  TimeDistortion,
           //  GravityWell,
           //  IonStorm,
           //  PlasmaVortex,
           //  SpaceHermit,
           //  BountyHunter,
           //  SpacePiranhas,
           //  LivingCrystal,
           //  VoidEchoes,
           //  DimensionalRift,
           //  HolographicDecoy,
           //  SpaceGeyser,
           //  MagneticAnomaly,
           //  CrystallineEntity,
           //  QuantumComputer,
           //  NanoswarmCloud,
           //  TachyonField,
           //  PsiOrbNetwork,
           //  SpaceMirage,
           //  CosmicStringFragment,
           //  DarkMatterNode,
           //  ASTEROIDHatcher,
           //  SpaceLeviathan,
           //  VoidKraken,
           //  StarforgeRemnant,
           //  TemporalLoop
}
impl Object {
  // pub const ARRAKIS:Self = Self::Planet { planet_type: (), radius: (), population: (), name: () }
  pub fn spawn_at(self, c: &mut Commands, pos: Vec3) -> Entity {
    let mut ec = c.spawn(Transform::from_translation(pos));
    Object::insert(&mut ec, self, ());
    ec.id()
  }
  pub const fn space_object(
    scale: f32,
    can_move: bool,
    visuals: Visuals,
    name: &'static str
  ) -> Self {
    Self::SpaceObject { scale, can_move, visuals, name }
  }
  pub const fn loot_object(
    scale: f32,
    name: &'static str,
    sprite: MySprite,
    item_type: Item
  ) -> Self {
    Self::LootObject { sprite, scale, name, item_type }
  }
  pub const fn npc(
    scale: f32,
    name: &'static str,
    speed: f32,
    faction: Faction,
    hp: u32,
    sprite: MySprite
  ) -> Self {
    Self::NPC { name, scale, faction, hp, speed, sprite }
  }
  pub fn insert(m: &mut EntityCommands, template: Self, extras: impl Bundle) {
    m.insert(extras);

    match template {
      Self::Empty => (),
      Self::SpaceObject { scale, can_move, visuals, name } => {
        let collider = Collider::sphere(1.0);
        Self::insert(
          m,
          Self::Empty,
          (
            SpaceObject { scale, ..default() },
            Name::new(name),
            FacingMode::Position,
            visuals,
            LockedAxes::ROTATION_LOCKED,
            ColliderMassProperties::from_shape(&collider, 1.0),
            collider,
            if can_move { RigidBody::Dynamic } else { RigidBody::Static },
            LinearDamping(1.6),
            AngularDamping(1.2),
            LinearVelocity::default(),
            AngularVelocity::default(),
            ExternalForce::default().with_persistence(false),
            ExternalImpulse::default(),
            Visibility::Visible
          )
        )
      }
      Self::Planet { sprite, radius, population, name } => {
        Self::insert(m, Self::space_object(radius, false, Visuals::sprite(sprite), name), ())
      }
      Self::TalkingPerson { name, sprite, dialogue_tree } => Self::insert(
        m,
        Self::space_object(1.7, true, Visuals::sprite(sprite), name),
        (Interact::dialogue_tree_default_state(dialogue_tree),)
      ),
      Self::ScaledNPC { scale, name, speed, faction, hp, sprite } => Self::insert(
        m,
        Self::space_object(scale, true, Visuals::sprite(sprite), name),
        (Navigation::speed(speed), NPC { follow_target: None, faction }, Combat {
          hp,
          ..default()
        })
      ),
      Self::NPC { name, hp, speed, sprite, scale, faction } => Self::insert(
        m,
        Self::space_object(scale, true, Visuals::sprite(sprite), name),
        (Navigation::speed(speed), NPC { follow_target: None, faction }, Combat {
          hp,
          ..default()
        })
      ),
      Self::Enemy { name, hp, speed, sprite } => Self::insert(
        m,
        Self::space_object(NORMAL_NPC_SCALE, true, Visuals::sprite(sprite), name),
        (
          Navigation::new(speed),
          NPC { follow_target: None, faction: Faction::SPACE_PIRATES },
          Combat { hp, is_hostile: true, ..default() }
        )
      ),
      Self::LootObject { sprite, scale, name, item_type } => Self::insert(
        m,
        Self::space_object(1.0, true, Visuals::sprite(sprite), name),
        (
          Name::new(format!("{:?}", item_type)),
          Interact::SingleOption(InteractSingleOption::Item(item_type))
        )
      ),
      Self::Explorer => Self::insert(
        m,
        Self::NPC {
          name: "explorer",
          hp: 50,
          speed: NORMAL_NPC_SPEED,
          sprite: MySprite::GPT4O_WHITE_EXPLORATION_SHIP,
          scale: NORMAL_NPC_SCALE,
          faction: Faction::EXPLORERS
        },
        ()
      ),
      Self::Trader => Self::insert(
        m,
        Self::ScaledNPC {
          scale: NORMAL_NPC_SCALE,
          name: "Trader",
          speed: NORMAL_NPC_SPEED,
          faction: Faction::TRADERS,
          hp: 30,
          sprite: MySprite::SPACESHIPWHITE2
        },
        ()
      ),
      Self::SpaceCop => Self::insert(
        m,
        Self::ScaledNPC {
          scale: NORMAL_NPC_SCALE,
          name: "space cop",
          speed: NORMAL_NPC_SPEED,
          faction: Faction::SPACE_POLICE,
          hp: 70,
          sprite: MySprite::GPT4O_POLICE_SPACE_SHIP
        },
        ()
      ),
      Self::SpaceWizard => Self::insert(
        m,
        Self::ScaledNPC {
          scale: NORMAL_NPC_SCALE,
          name: "space wizard",
          speed: NORMAL_NPC_SPEED,
          faction: Faction::SPACEWIZARDS,
          hp: 40,
          sprite: MySprite::IMAGEN3WIZARDSPACESHIP
        },
        ()
      ),
      Self::Miner => Self::insert(
        m,
        Self::ScaledNPC {
          scale: NORMAL_NPC_SCALE,
          name: "miner",
          speed: NORMAL_NPC_SPEED,
          faction: Faction::TRADERS,
          hp: 35,
          sprite: MySprite::GPT4O_MINING_SHIP
        },
        ()
      ),
      Self::AlienSoldier => Self::insert(
        m,
        Self::ScaledNPC {
          scale: NORMAL_NPC_SCALE,
          name: "alien soldier",
          speed: NORMAL_NPC_SPEED,
          faction: Faction::INVADERS,

          hp: 80,
          sprite: MySprite::PURPLEENEMYSHIP
        },
        ()
      ),
      Self::MinerNpc => Self::insert(
        m,
        Self::NPC {
          name: "miner npc",
          hp: 45,
          speed: NORMAL_NPC_SPEED * 0.8,
          sprite: MySprite::GPT4O_MINING_SHIP,
          scale: NORMAL_NPC_SCALE,
          faction: Faction::TRADERS
        },
        ()
      ),
      Self::MushroomMan => Self::insert(
        m,
        Self::ScaledNPC {
          scale: NORMAL_NPC_SCALE,
          name: "mushroom man",
          speed: NORMAL_NPC_SPEED * 0.9,
          faction: Faction::TRADERS,
          hp: 40,
          sprite: MySprite::MUSHROOMMAN
        },
        ()
      ),
      Self::SpaceCowboy => Self::insert(
        m,
        Self::space_object(
          NORMAL_NPC_SCALE,
          true,
          Visuals::sprite(MySprite::SPACECOWBOY),
          "space cowboy"
        ),
        (Interact::dialogue_tree_default_state(SPACE_COWBOY_DIALOGUE),)
      ),
      Self::Sign { text } => Self::insert(
        m,
        Self::space_object(
          1.5,
          false,
          Visuals::sprite(MySprite::SIGN),
          // const_format_args!()
          "sign"
        ),
        (
          Interact::SingleOption(InteractSingleOption::Describe),
          TextDisplay(text.to_string())
        )
      ),
      Self::Wormhole => Self::insert(
        m,
        Self::space_object(4.0, false, Visuals::sprite(MySprite::WORMHOLE), "wormhole"),
        (Interact::SingleOption(InteractSingleOption::Describe),)
      ),
      Self::Asteroid => Self::insert(
        m,
        Self::space_object(
          asteroid_scale(),
          false,
          Visuals::sprite(MySprite::GPT4O_ASTEROID),
          "Asteroid"
        ),
        (
          Interact::MultipleOptions(InteractMultipleOptions::AsteroidMiningMiniGame {
            resources_left: 5,
            tool_durability: 5
          }),
          CanBeFollowedByNPC
        )
      ),
      Self::SpaceCat => Self::insert(
        m,
        Self::loot_object(1.3, "space cat", MySprite::GPT4O_SPACE_CAT, Item::SPACECAT),
        ()
      ),
      Self::Spaceman => Self::insert(
        m,
        Self::loot_object(1.3, "spaceman", MySprite::GPT4O_SPACE_MAN, Item::PERSON),
        ()
      ),
      Self::SpaceCoin => Self::insert(
        m,
        Self::loot_object(1.7, "space coin", MySprite::COIN, Item::SPACECOIN),
        ()
      ),
      Self::IceAsteroid => Self::insert(
        m,
        Self::loot_object(
          asteroid_scale(),
          "ice asteroid",
          MySprite::GPT4O_ICE_ASTEROID,
          Item::DIHYDROGENMONOXIDE
        ),
        ()
      ),
      Self::CrystalAsteroid => Self::insert(
        m,
        Self::loot_object(
          asteroid_scale(),
          "crystal asteroid",
          MySprite::CRYSTALASTEROID,
          Item::CRYSTAL
        ),
        ()
      ),
      Self::CrystalMonster => Self::insert(
        m,
        Self::space_object(
          2.1,
          true,
          Visuals::sprite(MySprite::CRYSTALMONSTER),
          "crystal monster"
        ),
        (
          Combat { hp: 100, is_hostile: true, ..default() },
          NPC { faction: Faction::SPACE_PIRATES, ..default() },
          Navigation::speed(NORMAL_NPC_SPEED * 1.2)
        )
      ),
      Self::CrystalMonster2 => Self::insert(
        m,
        Self::space_object(
          1.7,
          true,
          Visuals::sprite(MySprite::CRYSTALMONSTER),
          "lesser crystal monster"
        ),
        (Interact::SingleOption(InteractSingleOption::Describe),)
      ),
      Self::HpBox => Self::insert(
        m,
        Self::space_object(0.9, true, Visuals::sprite(MySprite::HPBOX), "hp box"),
        (Interact::SingleOption(InteractSingleOption::HPBOX),)
      ),
      Self::TreasureContainer => Self::insert(
        m,
        Self::space_object(2.1, true, Visuals::sprite(MySprite::CONTAINER), "container"),
        (Interact::SingleOption(InteractSingleOption::CONTAINER(vec![
          (Item::SPACECOIN, 4),
          (Item::COFFEE, 1),
        ])),)
      ),
      Self::SphericalCow => Self::insert(
        m,
        Self::space_object(
          1.7,
          true,
          Visuals::sprite(MySprite::GPT4O_SPHERICAL_COW),
          "spherical cow"
        ),
        (Interact::dialogue_tree_default_state(SPHERICAL_SPACE_COW_DIALOGUE),)
      ),
      Self::TradeStation => {
        let (trade_interaction, text) = if prob(0.5) {
          let trade_buy =
            pick([Item::DIHYDROGENMONOXIDE, Item::CRYSTAL, Item::SPACECAT]).unwrap();
          (
            Interact::SingleOption(InteractSingleOption::Trade {
              inputs: (trade_buy, 1),
              outputs: (Item::SPACECOIN, 5)
            }),
            format!("space station\nbuys {:?}", trade_buy)
          )
        } else {
          let trade_sell = pick([Item::SPICE, Item::COFFEE, Item::ROCK]).unwrap();
          (
            Interact::SingleOption(InteractSingleOption::Trade {
              inputs: (Item::SPACECOIN, 5),
              outputs: (trade_sell, 1)
            }),
            format!("space station\nsells {:?}", trade_sell)
          )
        };
        Self::insert(
          m,
          Self::space_object(
            3.0,
            false,
            Visuals::sprite(MySprite::IMAGEN3SPACESTATION),
            "space station"
          ),
          (CanBeFollowedByNPC, trade_interaction, TextDisplay::from(text))
        )
      }
      Self::FloatingIsland => Self::insert(
        m,
        Self::space_object(
          3.4,
          false,
          Visuals::sprite(MySprite::FLOATINGISLAND),
          "floating island"
        ),
        (Interact::SingleOption(InteractSingleOption::Describe),)
      ),
      Self::Sun => Self::insert(
        m,
        Self::space_object(
          300.0,
          false,
          Visuals::material_sphere(MyMaterial::GLOWY_2),
          "sun"
        ),
        (CubemapVisibleEntities::default(), CubemapFrusta::default(), PointLight {
          intensity: 3_000_000.0,
          radius: 1.0,
          range: 10000.0,
          shadows_enabled: true,
          color: Color::srgb(0.9, 0.8, 0.6),
          ..default()
        })
      ),
      Self::AbandonedShip => Self::insert(
        m,
        Self::space_object(
          2.0,
          false,
          Visuals::sprite(MySprite::SPACESHIPABANDONED),
          "abandoned ship"
        ),
        (Interact::MultipleOptions(InteractMultipleOptions::Salvage { how_much_loot: 3 }),)
      ),
      Self::Player => {
        Self::insert(
          m,
          Self::space_object(
            PLAYER_SCALE,
            true,
            Visuals::sprite(MySprite::IMAGEN3WHITESPACESHIP),
            "You"
          ),
          (
            Player::default(),
            Combat { hp: 400, ..default() },
            Inventory::default(),
            Navigation::new(PLAYER_FORCE),
            CanBeFollowedByNPC
          )
        );
        println!("player spawned");
      }
      Self::SpacePirate => Self::insert(
        m,
        Self::Enemy {
          name: "Space Pirate",
          hp: 60,
          speed: NORMAL_NPC_SPEED * 1.1,
          sprite: MySprite::PURPLEENEMYSHIP
        },
        ()
      ),
      Self::SpacePirateBase => Self::insert(
        m,
        Self::space_object(
          4.0,
          false,
          Visuals::sprite(MySprite::GPT4O_PIRATE_STATION),
          "space pirate base"
        ),
        (
          Combat { hp: 120, is_hostile: false, ..default() },
          Interact::SingleOption(InteractSingleOption::Describe)
        )
      ),
      Self::SpaceStation => Self::insert(
        m,
        Self::space_object(
          4.0,
          false,
          Visuals::sprite(MySprite::GPT4O_TRADING_STATION),
          "space station"
        ),
        (
          Combat { hp: 120, is_hostile: false, ..default() },
          Interact::SingleOption(InteractSingleOption::Describe)
        )
      ),
      Self::Nomad => Self::insert(
        m,
        Self::npc(
          NORMAL_NPC_SCALE,
          "nomad",
          NORMAL_NPC_SPEED,
          Faction::WANDERERS,
          35,
          MySprite::GPT4O_GREEN_CAR_SHIP
        ),
        ()
      ),
      Self::WarpGate { name } => Self::insert(
        m,
        Self::space_object(3.0, false, Visuals::sprite(MySprite::GPT4O_GATE), name),
        (Interact::MultipleOptions(InteractMultipleOptions::WarpGate { name }), WarpGate)
      )
    }
  }
}

#[derive(Clone, Copy, Debug)]
pub struct Zone {
  pub name: &'static str,
  pub objects: &'static [([f32; 3], Object)],
  // each zone should has a warp gate at specified coords
  pub warp_gate_coords: [f32; 3],
  pub faction_control: Option<Faction>
}

pub fn from<B, A: From<B>>(b: B) -> A { A::from(b) }

fn rangerand(lo: f32, hi: f32) -> f32 { lo.lerp(hi, rand::random::<f32>()) }
fn random_zone_name() -> String {
  String::from_utf8((0..4).map(|_| rangerand(0.0, 30.0) as u8).collect()).unwrap()
  // (0..4).map(|_| random::<char>()).collect()
}
#[derive(Component, Clone)]
#[require(WarpGate)]
struct GatesConnected(Entity, Entity);
#[derive(Component, Clone, Default)]
struct WarpGate;
fn asteroid_scale() -> f32 { rangerand(1.8, 5.3) }
fn random_normalized_vector() -> Vec3 { random::<Quat>() * Vec3::X }
fn prob(p: f32) -> bool { p > random::<f32>() }

#[derive(Component, Clone)]
struct ZoneEntity {
  zone: Entity
}

impl Zone {
  fn spawn_at(
    self,
    mut c: &mut Commands,
    zone_pos: Vec3 // , zone_entity: Entity
  ) {
    let Self { name, objects, warp_gate_coords, faction_control } = self;
    for &(relpos, obj) in objects {
      obj.spawn_at(&mut c, zone_pos + Vec3::from_array(relpos));
    }
    Object::WarpGate { name }
      .spawn_at(&mut c, zone_pos + Vec3::from_array(warp_gate_coords));
  }
}

const MAX_ZONE_RANGE: f32 = 200.0;

// #[derive(Component, Debug, PartialEq, Eq, Clone)]
// pub struct InZone {
//   in_player_zone: bool // zone: Option<Entity>
// }

fn spawn_sprite(
  serv: Res<AssetServer>,
  // assets: Res<Assets>,
  mut c: Commands,
  mut oh: Local<Option<Handle<Image>>>,
  mut done: Local<bool>,
  // mut next_state: ResMut<NextState<GameState>>,
  mut sprite_params: Sprite3dParams
) {
  match &*oh {
    None => *oh = Some(serv.load("embedded://lava_planet.png")),
    Some(h) => {
      if (!*done) && matches!(serv.get_load_state(h.id()), Some(LoadState::Loaded)) {
        c.spawn(
          Sprite3dBuilder {
            image: h.clone(),
            pixels_per_metre: 0.02,
            double_sided: true,
            alpha_mode: AlphaMode::Blend,
            unlit: true,
            // pivot: Some(Vec2::new(0.5, 0.5)),
            ..default()
          }
          .bundle(&mut sprite_params)
        );
        *done = true;
      }
    }
  }
}
pub fn setup(
  playerq: Query<&Transform, With<Player>>,
  serv: Res<AssetServer>,
  mut meshes: ResMut<Assets<Mesh>>,
  mut materials: ResMut<Assets<StandardMaterial>>,
  mut c: Commands
) {
  let sun_pos = Vec3::ZERO;
  Object::Player.spawn_at(&mut c, Vec3::Y * 1000.0);
  Object::Sun.spawn_at(&mut c, sun_pos);
  for (pos, zone) in aigen::ZONES {
    zone.spawn_at(&mut c, Vec3::from_array(*pos));
  }

  let colorful_mat = serv.add(StandardMaterial::from(serv.add(colorful_texture())));
  c.spawn((
    PointLight { shadows_enabled: true, ..default() },
    Transform::from_xyz(4.0, 8.0, 4.0)
  ));

  let fov = std::f32::consts::PI / 4.0;

  let pitch_limit_radians = 1.0;
  let camera = (
    IsDefaultUiCamera,
    BLOOM,
    // Skybox { image: skybox_handle.clone(),
    //          brightness: 600.0 },
    Camera2d,
    // Camera3d { ..default() },
    // Camera { hdr: true,
    //          ..default() },
    // Projection::Perspective(PerspectiveProjection { fov, ..default() }),
    // bevy::render::camera::Exposure { ev100: 10.0 },
    // Transform::default(),
    Camera3dBundle {
      camera: Camera { hdr: true, ..default() },
      projection: Projection::Perspective(PerspectiveProjection { fov, ..default() }),
      exposure: bevy::render::camera::Exposure { ev100: 10.0 },
      // tonemapping:
      //   bevy::core_pipeline::tonemapping::Tonemapping::Reinhard,
      ..default()
    },
    PanOrbitCamera {
      // radius: Some(5.0),

      // focus: todo!(),
      // yaw: todo!(),
      // pitch: todo!(),
      // target_focus: todo!(),
      // target_yaw: todo!(),
      // target_pitch: todo!(),
      // target_radius: todo!(),
      // yaw_upper_limit: todo!(),
      // yaw_lower_limit: todo!(),
      pitch_upper_limit: Some(pitch_limit_radians),
      pitch_lower_limit: Some(-pitch_limit_radians),
      zoom_upper_limit: Some(200.0),
      zoom_lower_limit: 5.0,
      // orbit_sensitivity: todo!(),
      orbit_smoothness: 0.0,
      pan_sensitivity: 0.0,
      pan_smoothness: 0.85,
      zoom_sensitivity: 2.5,
      // zoom_smoothness: todo!(),
      // button_orbit: todo!(),
      // button_pan: todo!(),
      // modifier_orbit: todo!(),
      // modifier_pan: todo!(),
      // touch_enabled: todo!(),
      // touch_controls: todo!(),
      // reversed_zoom: todo!(),
      // is_upside_down: todo!(),
      // allow_upside_down: todo!(),
      // enabled: todo!(),
      // initialized: todo!(),
      // force_update: todo!(),
      ..default()
    }
  );
  c.spawn(camera);
  println("spawned camera");
  println("setup");
}

fn spawn_skybox(
  serv: Res<AssetServer>,
  mut images: ResMut<Assets<Image>>,
  mut camq: Query<Entity, (With<Camera>, Without<Skybox>)>,
  mut c: Commands,
  mut skybox_handle: Local<Option<Handle<Image>>>
) {
  if let Ok(cam_entity) = camq.get_single() {
    // Image::reinterpret_stacked_2d_as_array(&mut self, layers);
    let skybox_handle = serv.load(MySprite::SKYBOX.embedded_path());
    if let Some(mut skybox) = images.get_mut(&skybox_handle) {
      println("hmm2");
      // skybox.reinterpret_size(Extent3d { width: skybox.width() / 3,
      //                                    height: skybox.height() / 2,
      //                                    depth_or_array_layers: 6 });
      skybox.reinterpret_stacked_2d_as_array(skybox.height() / skybox.width());

      skybox.texture_view_descriptor = Some(TextureViewDescriptor {
        dimension: Some(bevy::render::render_resource::TextureViewDimension::Cube),
        ..default()
      });
      c.entity(cam_entity).insert(Skybox {
        image: skybox_handle.clone(),
        brightness: 600.0,
        rotation: default()
      });
      // *done = true;
    } else {
      c.spawn(Skybox { image: skybox_handle, ..default() });
    }
  }
}
#[bevy_main]
pub fn main() {
  let gravity = avian3d::dynamics::integrator::Gravity::ZERO;
  let solver_config = SolverConfig {
    contact_damping_ratio: 0.5,
    // contact_frequency_factor: 1.5,
    // max_overlap_solve_speed: 4.0,
    // warm_start_coefficient: 1.0,
    // restitution_threshold: 1.0,
    // restitution_iterations: 1,
    ..default()
  };
  let address_mode = bevy::image::ImageAddressMode::ClampToBorder;
  let default_sampler = bevy::image::ImageSamplerDescriptor {
    // address_mode_u: address_mode,
    //                        address_mode_v: address_mode,
    //                        address_mode_w: address_mode,
    mag_filter: ImageFilterMode::Nearest,
    min_filter: ImageFilterMode::Linear,
    mipmap_filter: ImageFilterMode::Linear,
    // compare:
    //   Some(ImageCompareFunction::Less),
    // lod_min_clamp: 10.0,
    // lod_max_clamp: 100.0,
    // border_color:
    //   Some(ImageSamplerBorderColor::TransparentBlack),
    // anisotropy_clamp: 1000,
    ..default()
  };
  App::new()
    .add_plugins((
      EmbeddedAssetPlugin::default(),
      // bevy::pbr::ScreenSpaceAmbientOcclusionPlugin
      DefaultPlugins
        // .set(RenderPlugin {
        //   render_creation: bevy::render::settings::RenderCreation::Automatic(bevy::render::settings::WgpuSettings {
        //     backends: Some(bevy::render::settings::Backends::VULKAN),
        //     ..default()
        //   }),
        //   ..default()
        // })
        .set(ImagePlugin { default_sampler })
        .set(WindowPlugin {
          primary_window: Some(Window {
            // resolution: WindowResolution
            mode: WindowMode::Windowed,

            present_mode: bevy::window::PresentMode::AutoVsync,
            title: "bevy space game".to_string(),
            canvas: Some("#bevy".to_string()),
            ..default()
          }),
          ..default()
        }),
      HaalkaPlugin,
      // bevy_vox_scene::VoxScenePlugin,
      bevy_sprite3d::Sprite3dPlugin,
      bevy_panorbit_camera::PanOrbitCameraPlugin,
      // bevy_mod_billboard::prelude::BillboardPlugin,
      avian3d::PhysicsPlugins::default() // QuillPlugin,
                                         // QuillOverlaysPlugin,
    ))
    // .add_event::<GuiInputEvent>()
    .init_resource::<UIData>()
    .init_resource::<TimeTicks>()
    .insert_resource(gravity)
    .insert_resource(solver_config)
    .insert_resource(ClearColor(Color::BLACK))
    // .init_asset::<bevy_vox_scene::VoxelScene>()
    .insert_resource(AMBIENT_LIGHT)
    .add_systems(
      Startup,
      (
        setup,
        setup_ui_system // ,add_global_highlight
                        ,// ui
      )
        .chain()
    )
    // .add_systems(Startup, setup.run_if(in_state))
    .add_systems(
      Update,
      (
        // spawn_sprite,
        close_on_esc,
        // spawn_mushroom_man,
        player_movement.param_warn_once(),
        camera_follow_player.param_warn_once(),
        combat_system.param_warn_once(),
        visuals,
        face_camera_system.param_warn_once(),
        warp.param_warn_once(),
        // timed_animation_system,
        player_target_interaction.param_warn_once()
      )
        .chain()
    )
    .add_systems(
      Update,
      (
        // update_in_zone,
        ui,
        // spawn_skybox,
        npc_movement,
        set_space_object_scale,
        interact,
        combat_visual_effects,
        origin_time,
        increment_time,
        navigation,
        click_target,
        // face_camera_dir,
        // face_camera,
        // set_visuals,
        debug_input
      )
        .chain()
    )
    .run();
}

// trunk build --release --public-url "bevyspacegame" --filehash false

// trunk serve

// cargo check --target wasm32-unknown-unknown
// cargo run --target x86_64-unknown-linux-gnu
// cargo check --target x86_64-unknown-linux-gnu

enum Temperature {
  Permafrost,
  Cold,
  Medium,
  Warm,
  Hot
}
pub struct Region {
  is_mountain: bool,
  temperature: Temperature,
  land_area: f32
}
enum Ideology {
  Communism,
  Fascism,
  Democracy
}
pub struct Country {
  regions: Vec<Region>,
  ideology: Ideology
}
pub struct WorldMap(Vec<Country>);
// pub fn integration_parameters() -> IntegrationSet {
//     IntegrationParameters { damping_ratio: 4.0,
//                             // dt: todo!(),
//                             // min_ccd_dt: todo!(),
//                             // erp: todo!(),
//                             // joint_erp: todo!(),
//                             // joint_damping_ratio: todo!(),
//                             // warmstart_coefficient: todo!(),
//                             // length_unit: todo!(),
//                             // normalized_allowed_linear_error: todo!(),
//                             // normalized_max_penetration_correction: todo!(),
//                             // normalized_prediction_distance: todo!(),
//                             // num_solver_iterations: todo!(),
//                             // num_additional_friction_iterations: todo!(),
//                             // num_internal_pgs_iterations: todo!(),
//                             // num_internal_stabilization_iterations: todo!(),
//                             // min_island_size: todo!(),
//                             // max_ccd_substeps: todo!(),
//                             ..default() }
//   }

comment! {
  fn update_in_zone(player_transform: Single<&Transform, With<Player>>,
                    mut entity_query: Query<(Entity, &Transform, Option<&mut InZone>),
                                            Without<Player>>,
                    zone_query: Query<(Entity, &Zone, &Transform)>,
                    mut c: Commands) {
    let player_zone = find(|(zone_entity, zone, zone_transform)| {
      player_transform.translation
                      .distance(zone_transform.translation)
        < MAX_ZONE_RANGE
    },
                           &zone_query);

    for (entity, transform, mut in_zone) in entity_query.iter_mut() {
      let new_in_zone =
        InZone { in_player_zone:
                 player_zone.map_or(false, |(zone_entity, zone, zone_transform)| {
                   zone_transform.translation.distance(transform.translation)
                     < MAX_ZONE_RANGE
                 }) };
      match in_zone {
        Some(mut in_zone) => {
          if *in_zone != new_in_zone {
            *in_zone = new_in_zone;
          }
        }
        None => {
          c.entity(entity).insert(new_in_zone);
        }
      }
    }
  }
}
