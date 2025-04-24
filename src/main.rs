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
use bevy::{pbr::{MaterialPipeline, MaterialPipelineKey},
           render::{mesh::{MeshVertexBufferLayout, MeshVertexBufferLayoutRef},
                    render_resource::{AsBindGroup, CompareFunction, Face,
                                      RenderPipelineDescriptor, ShaderRef,
                                      SpecializedMeshPipelineError}}};
use {aigen::*,
     avian3d::prelude::*,
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
     bevy::{asset::LoadState,
            core_pipeline::Skybox,
            render::render_resource::{Extent3d, TextureViewDescriptor}},
     bevy_embedded_assets::*,
     // bevy_mod_billboard::{BillboardDepth, BillboardLockAxis, BillboardMeshHandle,
     //                      BillboardTextBundle, BillboardTextureBundle,
     //                      BillboardTextureHandle}
     // ,
     bevy_panorbit_camera::PanOrbitCamera,
     bevy_sprite3d::Sprite3dBuilder,
     bevy_sprite3d::Sprite3dParams,
     // bevy_quill::{prelude::*, QuillPlugin, ViewChild},
     // bevy_quill_overlays::QuillOverlaysPlugin,
     dynamics::solver::SolverConfig,
     fancy_constructor::new,
     haalka::prelude::*,
     rand::{Rng, random, thread_rng},
     rust_utils::mapv,
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

  const GPT4O_ALIEN_ARTIFACT: Self = Self::new("GPT4O alien artifact.png");
  const GPT4O_DRONE: Self = Self::new("GPT4O drone.png");
  const GPT4O_SIGN: Self = Self::new("GPT4O sign.png");
  const GPT4O_SIGNAL_RELAY_SATELLITE: Self = Self::new("GPT4O signal relay satellite.png");
  const GPT4O_WORMHOLE: Self = Self::new("GPT4O wormhole.png");

  const IMAGEN3GREENSPACESHIP: Self = Self::new("imagen3greenspaceship.png");
  const IMAGEN3WIZARDSPACESHIP: Self = Self::new("imagen3wizardspaceship.png");
  const IMAGEN3WHITESPACESHIP: Self = Self::new("imagen3whitespaceship.png");
  const IMAGEN3SPACESTATION: Self = Self::new("imagen3spacestation.png");
  const IMAGEN3FLOATINGISLAND: Self = Self::new("imagen3floatingisland.png");
  const ASTEROID: Self = Self::new("asteroid.png");
  const BLOCKTEXTURES: Self = Self::new("pixelc/block_textures.png");
  const BRICKS: Self = Self::new("pixelc/bricks.png");
  const BROWNGASGIANT: Self = Self::new("pixelc/browngasgiant.png");
  // const CAR: Self = Self::new("car.png");
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

#[derive(Component, Clone, PartialEq, Eq, Default)]
pub struct TextDisplay(pub String);

impl<T: ToString> From<T> for TextDisplay {
  fn from(value: T) -> Self { Self(value.to_string()) }
}

#[derive(Component, Clone, Copy, PartialEq, Eq, Debug)]
pub enum Visuals {
  Sprite(MySprite),
  MaterialMesh { material: MyMaterial, mesh: GenMesh } // None
}

impl Visuals {
  // fn none() -> Self{Self::None}
  const fn sprite(sprite: MySprite) -> Self { Self::Sprite(sprite) }
  const fn material_mesh(material: MyMaterial, mesh: GenMesh) -> Self {
    Self::MaterialMesh { material, mesh }
  }
  const fn material_sphere(material: MyMaterial) -> Self {
    Self::material_mesh(material, GenMesh::SPHERE)
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
  let billboard_mesh_handle = get_mesh_handle(GenMesh::BILLBOARD_MESH_SQUARE);
  let invisible_material = get_material_handle(MyMaterial::INVISIBLE);
  // Children

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
  pub target_interaction_state: Option<PlayerTargetInteractionState>
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

// #[derive(Clone)]
pub struct MyCommand(pub Box<dyn FnOnce(&mut World) + 'static + Send + Sync>);
type CMD = MyCommand;

impl<F: FnOnce(&mut World) + 'static + Send + Sync> From<F> for MyCommand {
  fn from(f: F) -> Self { MyCommand(Box::new(f)) }
}
impl From<Vec<MyCommand>> for MyCommand {
  fn from(cmds: Vec<MyCommand>) -> Self {
    MyCommand::from(|w: &mut World| {
      for cmd in cmds {
        cmd.0(w);
      }
    })
  }
}
impl<const N: usize> From<[MyCommand; N]> for MyCommand {
  fn from(cmds: [MyCommand; N]) -> Self {
    MyCommand::from(|w: &mut World| {
      for cmd in cmds {
        cmd.0(w);
      }
    })
  }
}

impl Command for MyCommand {
  fn apply(self, world: &mut World) { (self.0)(world); }
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

  pub fn spawn_at(o: Object, pos: Vec3) -> Self {
    (move |world: &mut World| {
      let mut commands = world.commands();
      o.spawn_at(&mut commands, pos);
    })
    .into()
  }

  pub fn give_item_to_player(item: Item) -> Self {
    (move |world: &mut World| {
      if let Some(player_entity) = world.get_player() {
        world.mutate_component(player_entity, |inventory: &mut Inventory| {
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
    (move |world: &mut World| world.insert_component(entity, component)).into()
  }

  pub fn update_component<C: Component + Clone + 'static>(
    entity: Entity,
    f: impl FnOnce(C) -> C + 'static + Send + Sync
  ) -> Self {
    (move |world: &mut World| world.update_component(entity, f)).into()
  }

  pub fn mutate_component<C: Component + 'static>(
    entity: Entity,
    f: impl FnOnce(&mut C) + 'static + Send + Sync
  ) -> Self {
    (move |world: &mut World| world.mutate_component(entity, f)).into()
  }

  pub fn insert_player_component<C: Component + 'static>(component: C) -> Self {
    (move |world: &mut World| {
      if let Some(player_entity) = world.get_player() {
        world.insert_component(player_entity, component);
      }
    })
    .into()
  }

  pub fn update_player_component<C: Component + Clone + 'static>(
    f: impl FnOnce(C) -> C + 'static + Send + Sync
  ) -> Self {
    (move |world: &mut World| {
      if let Some(player_entity) = world.get_player() {
        world.update_component(player_entity, f);
      }
    })
    .into()
  }

  pub fn mutate_player_component<C: Component + Clone + 'static>(
    f: impl FnOnce(&mut C) + 'static + Send + Sync
  ) -> Self {
    (move |world: &mut World| {
      if let Some(player_entity) = world.get_player() {
        world.mutate_component(player_entity, f);
      }
    })
    .into()
  }
}

pub trait WorldExt {
  fn get_world_mut(&mut self) -> &mut World;
  fn get_world(&self) -> &World;
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
  fn get_player(&self) -> Option<Entity> {
    self
      .get_world()
      .iter_entities()
      .find_map(|er| er.contains::<Player>().then_some(er.id()))
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
  fn get_player_pos(&self) -> Option<Vec3> {
    let world = self.get_world();
    let player = world.get_player()?;
    let &transform = world.get_entity(player).ok()?.get_components::<&Transform>()?;
    Some(transform.translation)
  }
  fn get_component<C: Component + 'static>(&self, entity: Entity) -> Option<&C> {
    self.get_world().get::<C>(entity)
  }
  fn get_player_component<C: Component + 'static>(&self) -> Option<&C> {
    let player_entity = self.get_player()?;
    self.get_component(player_entity)
  }
}
impl WorldExt for World {
  fn get_world_mut(&mut self) -> &mut World { self }
  fn get_world(&self) -> &World { self }
}

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
  mut hostileq: Query<(Entity, &Combat, &Transform), Without<Player>>,
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
    // todo!("fix this");
    println("pressed t");
    if let Some((e, _, _)) = filter_least(
      |(e, combat, transform)| {
        combat.is_hostile.then_some(transform.translation.distance(player_pos) as u32)
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
      }
      NavigationKind::Vec3(v) => {
        accelerate(v);
      }
      NavigationKind::Pos(pos) => {
        let v = (pos - translation);
        accelerate(v);
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
        }
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

struct InteractionInput<'t> {
  // current_option: u8,
  // selected: Option<u8>,
  // display: String,
  world: &'t World,
  // self_component: &'t T,
  self_entity: Entity
}
#[derive(Default)]
pub struct InteractionOutput {
  pub choices: Vec<(String, MyCommand)>,
  pub msg: String
}

impl InteractionOutput {
  pub fn new() -> Self { default() }
  fn msg(self, text: impl ToString) -> Self { Self { msg: text.to_string(), ..self } }
  pub fn add(mut self, label: impl ToString, cmd: impl Into<MyCommand>) -> Self {
    self.choices.push((label.to_string(), cmd.into()));
    self
  }
  pub fn add_if(
    mut self,
    cond: bool,
    label: impl ToString,
    cmd: impl Into<MyCommand>
  ) -> Self {
    if cond {
      self.choices.push((label.to_string(), cmd.into()));
    }
    self
  }
}

comment! {
  impl<'t> InteractionInput<'t> {
    fn add_option(&mut self, label: impl ToString, effect: impl Into<MyCommand>) -> &mut Self {
      if self.selected == Some(self.current_option) {
        self.effect = Some(effect.into())
      }
      self.current_option += 1;
      self
    }

    fn selected(&mut self) -> Option<u8> {
      let sel = self.selected;
      self.current_option = self.current_option.wrapping_add(1);
      sel
    }
  }
  fn play_mini_game(
    world: &mut World,
    mut playerq: Single<(Entity, &mut Transform, &Combat, &Inventory), With<Player>>,
    mut c: Commands,
    keys: Res<ButtonInput<KeyCode>>
  ) {
    let keys = world.resource::<ButtonInput<KeyCode>>();
    let just_pressed = keys.get_just_pressed();
    let games: Vec<(Entity, &Transform, &mut MiniGame, Option<&Name>)> =
      world.query_filtered().iter(world).collect();
    let player = world.get_player().unwrap();
    let player_pos = world.get_player_pos().unwrap();
    let c = world.commands();
    // let (player_transform, player_combat, player_inventory) = world
    //   .get_entity(player)
    //   .unwrap()
    //   .get_components::<(&Transform, &Combat, &Inventory)>()
    //   .unwrap();
    // let (player, player_transform, player_combat, player_inventory) = playerq.into_inner();
    // let player_pos = player_transform.translation;
    let closest_interactable_thing = filter_least(
      |&&(e, t, g, oname)| {
        let dist = t.translation.distance(player_pos);
        (dist < INTERACTION_RANGE).then_some(dist as u32)
      },
      &games
    );
    if let Some((interact_entity, transform, mut game, oname)) = closest_interactable_thing {
      let ctx = MiniGameContext {
        world,
        selected_option_number: None,
        current_option_number: 0,
        display_string: "mini-game object".to_string()
      };
      game.0.play_mini_game(&mut ctx);

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
      let (msg, options) = interact_multiple_options.clone().interact();
      INTERACT_MESSAGE.set(Some(intersperse_newline([msg, default()].into_iter().chain(
        (&options).into_iter().enumerate().map(|(n, tup)| format!("{}: {}", n + 1, tup.0))
      ))));
      for (n, (string, command, new_interact)) in options.into_iter().enumerate() {
        if number_picked == Some(n as u8 + 1) {
          c.queue(command);
          *interact_multiple_options = new_interact.clone();
        }
      }

      INTERACT_MESSAGE.set(Some(format!("[SPACE: {message}]")));
      if keys.just_pressed(KeyCode::Space) {
        c.queue(command);
      }
    } else {
      INTERACT_MESSAGE.set(None);
    }
  }
}

const INTERACTION_RANGE: f32 = 8.0;

#[derive(Component)]
struct Salvage {
  loot: u8
}

#[derive(Component, Clone, Copy, Debug)]
pub struct InteractImpl(pub fn(&World, Entity) -> InteractionOutput);
impl InteractImpl {
  const SALVAGE: InteractImpl = InteractImpl(|w, e| {
    let &Salvage { loot } = w.get::<Salvage>(e).unwrap();
    InteractionOutput::new()
      .msg("Salvage wreck")
      .add("leave", CMD::message_add("You leave"))
      .add_if(loot > 0, "take loot", [
        CMD::message_add("Looted"),
        CMD::give_item_to_player(Item::SPACECOIN),
        CMD::mutate_component::<Salvage>(e, |mut s| s.loot -= 1)
      ])
  });
  const DIALOGUE_TREE: InteractImpl = InteractImpl(|w, e| {
    let dialogue = w.get::<DialogueTreeInteract>(e).unwrap();
    let display_name = w.get::<Name>(e).cloned().unwrap_or(Name::new("talking NPC"));

    let node_opts = dialogue
      .tree
      .iter()
      .find(|&&(id, _)| id == dialogue.current)
      .map(|&(_, opts)| opts)
      .unwrap();

    node_opts.iter().fold(
      InteractionOutput::new().msg(display_name),
      |out, &(next, player_line, npc_line, effect)| {
        let cmd = MyCommand::from(match effect {
          Some(eff) => vec![
            CMD::message_add(npc_line),
            CMD::mutate_component::<DialogueTreeInteract>(e, move |c| c.current = next),
            eff(),
          ],
          None => vec![
            CMD::message_add(npc_line),
            CMD::mutate_component::<DialogueTreeInteract>(e, move |c| c.current = next),
          ]
        });
        out.add(player_line, cmd)
      }
    )
  });
  const HP_BOX: InteractImpl = InteractImpl(|w, e| {
    InteractionOutput::new().msg("Health Box").add("Use health box", [
      CMD::message_add("Health restored"),
      CMD::mutate_player_component::<Combat>(|c| c.hp += 20),
      CMD::despawn_entity(e)
    ])
  });
  const DESCRIBE: InteractImpl = InteractImpl(|w, e| {
    let display_name = namefmt(w.get::<Name>(e));
    let display_text = w.get::<TextDisplay>(e).cloned().unwrap_or_default().0;
    InteractionOutput::new().msg(format!("{display_name}: {display_text}"))
  });
  // const ITEM_PICKUP: InteractImpl = InteractImpl(|w, e| {
  //   let container = w.get::<Container>(e).unwrap();

  //   InteractionOutput::new()
  //     .msg(format!("You found: {}", container.label))
  //     .add("Take item", [
  //       CMD::give_item_to_player(container.item.clone()),
  //       CMD::message_add(format!("You picked up {}", container.label)),
  //       CMD::despawn_entity(e)
  //     ])
  //     .add("Leave it", CMD::none())
  // });
  const TRADE: InteractImpl = InteractImpl(|w, e| {
    let trade = w.get::<TradeInteract>(e).unwrap();
    let inputs = trade.inputs.clone();
    let outputs = trade.outputs.clone();

    InteractionOutput::new().msg("Trade terminal").add("trade", [
      CMD::mutate_player_component::<Inventory>(move |inv| {
        inv.trade([inputs.clone()], [outputs.clone()])
      }),
      CMD::message_add("Trade executed")
    ])
  });
  const CONTAINER: InteractImpl = InteractImpl(|w, e| {
    let container = w.get::<Container>(e).unwrap();
    let oname = w.get::<Name>(e);
    let display_name = namefmt(oname);
    let loot = container.0.clone();

    InteractionOutput::new().msg(display_name.clone()).add("take", [
      CMD::despawn_entity(e),
      CMD::mutate_player_component::<Inventory>(move |inv| inv.add_contents(loot.clone())),
      CMD::message_add(format!("{} looted", display_name))
    ])
  });

  // --- ASTEROID MINING ---
  const ASTEROID_MINING: InteractImpl = InteractImpl(|w, e| {
    let mining = w.get::<AsteroidMining>(e).unwrap();
    let can_mine = mining.resources > 0 && mining.durability > 0;

    InteractionOutput::new()
      .msg(format!("Mining asteroid: {} res, {} dur", mining.resources, mining.durability))
      .add_if(can_mine, "mine safe", [
        CMD::message_add("Safe mine"),
        CMD::give_item_to_player(Item::SPACEMINERALS),
        CMD::mutate_component::<AsteroidMining>(e, |mut a| a.resources -= 1)
      ])
      .add_if(can_mine, "mine hard", [
        CMD::message_add("Hard mine"),
        CMD::give_item_to_player(Item::SPACEMINERALS),
        CMD::give_item_to_player(Item::SPACEMINERALS),
        CMD::mutate_component::<AsteroidMining>(e, |a| {
          a.resources -= 1;
          a.durability -= 1;
        })
      ])
      .add("leave", CMD::none())
  });

  // --- WARP GATE ---
  const WARP_GATE: InteractImpl = InteractImpl(|w, e| {
    let player_pos = w.get_player_pos().unwrap();
    let mut gates: Vec<(&WarpGate, Vec3)> = w
      .iter_entities()
      .filter_map(|er| {
        er.get_components::<(&WarpGate, &Transform)>().map(|(wg, tr)| (wg, tr.translation))
      })
      .collect();

    gates.sort_by(|a, b| {
      a.1.distance(player_pos).partial_cmp(&b.1.distance(player_pos)).unwrap()
    });
    gates.truncate(4);

    gates
      .into_iter()
      .fold(InteractionOutput::new().msg("Select warp gate:"), |out, (gate, pos)| {
        out.add(gate.name, [
          CMD::message_add(format!("Warping to {}", gate.name)),
          CMD::mutate_player_component::<Transform>(move |t| t.translation = pos)
        ])
      })
      .add("leave", CMD::none())
  });
}
/*













*/

type DialogueEffect = fn() -> MyCommand;
type DialogueTreeNode = (
  &'static str,
  &'static [(&'static str, &'static str, &'static str, Option<DialogueEffect>)]
);
const DIALOGUE_END: DialogueTreeNode = ("END", &[]);
type DialogueTree = &'static [DialogueTreeNode];

// --- Dialogue Tree ---
#[derive(Component)]
pub struct DialogueTreeInteract {
  pub tree: DialogueTree,
  pub current: &'static str
}

impl DialogueTreeInteract {
  pub fn new(tree: DialogueTree) -> Self {
    let (current, _) = tree[0];
    Self { tree, current }
  }
}

struct HPBoxInteract {
  heal: u32
}
struct DescribeInteract {
  description: String
}
// #[derive(Component)]
// struct Loot {
//   item: Item,
//   label: String
// }

// --- Trade ---
#[derive(Component)]
pub struct TradeInteract {
  pub inputs: (Item, u32),
  pub outputs: (Item, u32)
}

#[derive(Component)]
pub struct Container(Vec<(Item, u32)>);

// --- Asteroid Mining ---
#[derive(Component)]
pub struct AsteroidMining {
  pub resources: u8,
  pub durability: u8
}

#[derive(Component)]
pub struct WarpGate {
  pub name: &'static str
}

fn interact(
  world: &World,
  mut playerq: Single<(Entity, &Transform), With<Player>>,
  mut interactable_q: Query<
    (Entity, &Transform, &InteractImpl, Option<&Name>),
    Without<Player>
  >,
  mut c: Commands,
  keys: Res<ButtonInput<KeyCode>>
) {
  let (player, &player_transform) = *playerq;
  let player_pos = player_transform.translation;
  let closest_interactable_thing = filter_least(
    |tup| {
      let dist = tup.1.translation.distance(player_pos);
      (dist < INTERACTION_RANGE).then_some(dist as u32)
    },
    &interactable_q
  );

  if let Some((interact_entity, transform, interact_impl, oname)) =
    closest_interactable_thing
  {
    let InteractionOutput { mut choices, msg } = (interact_impl.0)(world, interact_entity);

    let single_choice = choices.len() == 1;
    let number_picked = if single_choice && keys.just_pressed(KeyCode::Space) {
      Some(1)
    } else {
      find_map(|(n, key): (u8, KeyCode)| keys.just_pressed(key).then_some(n), [
        // (0, KeyCode::Digit0),
        (1u8, KeyCode::Digit1),
        (2, KeyCode::Digit2),
        (3, KeyCode::Digit3),
        (4, KeyCode::Digit4),
        (5, KeyCode::Digit5),
        (6, KeyCode::Digit6),
        (7, KeyCode::Digit7),
        (8, KeyCode::Digit8),
        (9, KeyCode::Digit9)
      ])
    };
    let choices_list = if single_choice {
      vec![format!("SPACE: {}", choices[0].0)]
    } else {
      mapv(|(i, (s, cmd))| format!("{}: {}", i + 1, s), choices.iter().enumerate())
    };
    INTERACT_MESSAGE.set(Some(intersperse_newline(
      [namefmt(oname), msg, default()].into_iter().chain(choices_list)
    )));
    if let Some(n) = number_picked {
      let index = n - 1;
      if index < choices.len() as u8 {
        c.queue(choices.remove(index as usize).1);
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
#[derive(bevy::ecs::query::QueryData)]
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
    enum Asdf {
      Asd { k: i32 }
    }
    let k = Asdf::Asd { k: 5 };
    // match k {
    //   Asdf::Asd { k: i32 } => {}
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
// const INC2: fn(i32) -> i32 = rust_utils::comp(rust_utils::inc, rust_utils::inc);

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

pub trait ObjectSpec {
  fn insert(&self, ec: &mut EntityCommands);
}
#[macro_export]
macro_rules! object_spec {
  (
    $spec_name:ident {
      $($field_name:ident : $field_ty:ty),* $(,)?
    },
    $body:expr,$extra:expr
  ) => {
    #[derive(Debug, Clone,Copy)]
    pub struct $spec_name {
      $(pub $field_name : $field_ty),*
    }
    impl ObjectSpec for $spec_name {
      fn insert(&self, ec: &mut EntityCommands){
        let &$spec_name{ $($field_name),*} = self;
        let body = $body;
        ObjectSpec::insert(&body,ec);
        // body.insert(ec);
        let extra = $extra;
        ec.insert(extra);
      }
    }
  };
}

#[derive(Debug, Clone, Copy)]
pub struct EmptySpec {}
impl ObjectSpec for EmptySpec {
  fn insert(&self, ec: &mut EntityCommands) {}
}
object_spec!(
SpaceObjectSpec {
  name: &'static str,
  scale: f32,
  can_move: bool,
  visuals: Visuals
},
  EmptySpec{},
{
  let collider = Collider::sphere(1.0);
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
  )});

pub const fn space_object(
  scale: f32,
  can_move: bool,
  visuals: Visuals,
  name: &'static str
) -> SpaceObjectSpec {
  SpaceObjectSpec { scale, can_move, visuals, name }
}
object_spec!(
  NPCSpec {
    name: &'static str,
    scale: f32,
    speed: f32,
    faction: Faction,
    hp: u32,
    sprite: MySprite
  },
  SpaceObjectSpec {
    name,
    scale,
    can_move: true,
    visuals: Visuals::sprite(sprite)
  },
  (Navigation::speed(speed),
   NPC { follow_target: None, faction },
   Combat { hp, is_hostile: false, ..default() }));
object_spec!(
  LootObjectSpec {
    scale: f32,
    name: &'static str,
    sprite: MySprite,
    item_type: Item // Assuming Item is Copy
  },
  SpaceObjectSpec{name,scale,can_move:true,visuals:Visuals::sprite(sprite)},
  (Container(vec![(item_type, 1)]), // Use item_type field
   InteractImpl::CONTAINER));

// Helper constructor for LootObjectSpec
pub const fn loot_object(
  scale: f32,
  name: &'static str,
  sprite: MySprite,
  item_type: Item
) -> LootObjectSpec {
  LootObjectSpec { scale, name, sprite, item_type }
}

// Enemy Spec
object_spec!(
  EnemySpec {
    name: &'static str,
    hp: u32,
    speed: f32,
    sprite: MySprite
  },
  // Body: Base on SpaceObjectSpec (movable, uses constant scale)
  space_object(NORMAL_NPC_SCALE, true, Visuals::sprite(sprite), name),
  // Extra: Navigation, NPC (hostile pirate faction), Combat (hostile)
  (
    Navigation::new(speed), // Use speed field
    NPC { follow_target: None, faction: Faction::SPACE_PIRATES }, // Fixed faction
    Combat { hp, is_hostile: true, ..default() } // Use hp field, explicitly hostile
  )
);

// Helper constructor for EnemySpec
pub const fn enemy(name: &'static str, hp: u32, speed: f32, sprite: MySprite) -> EnemySpec {
  EnemySpec { name, hp, speed, sprite }
}

// Talking Person Spec
object_spec!(
  TalkingPersonSpec {
    name: &'static str,
    sprite: MySprite,
    dialogue_tree: DialogueTree // Assuming DialogueTree is Copy
  },
  // Body: Base on SpaceObjectSpec (movable, fixed scale 1.7)
  space_object(1.7, true, Visuals::sprite(sprite), name),
  // Extra: Dialogue and Interaction components
  (
    DialogueTreeInteract::new(dialogue_tree), // Use dialogue_tree field
    InteractImpl::DIALOGUE_TREE
  )
);

// Helper constructor for TalkingPersonSpec
pub const fn talking_person(
  name: &'static str,
  sprite: MySprite,
  dialogue_tree: DialogueTree
) -> TalkingPersonSpec {
  TalkingPersonSpec { name, sprite, dialogue_tree }
}

// Planet Spec
object_spec!(
  PlanetSpec {
    sprite: MySprite,
    radius: f32,
    population: u32,
    name: &'static str
  },
  // Body: Base on SpaceObjectSpec (static/not movable)
  space_object(radius, false, Visuals::sprite(sprite), name),
  // Extra: Planet component
  (
    Planet { population }, // Use population field
  )
);

// Helper constructor for PlanetSpec
pub const fn planet(
  sprite: MySprite,
  radius: f32,
  population: u32,
  name: &'static str
) -> PlanetSpec {
  PlanetSpec { sprite, radius, population, name }
}

// Sign Spec
object_spec!(
  SignSpec {
    text: &'static str
  },
  // Body: Base on SpaceObjectSpec (static, fixed scale/sprite/name)
  space_object(1.5, false, Visuals::sprite(MySprite::GPT4O_SIGN), "sign"),
  // Extra: Interaction and TextDisplay components
  (
    InteractImpl::DESCRIBE,
    TextDisplay(text.to_string()) // Use text field
  )
);

pub const fn sign(text: &'static str) -> SignSpec { SignSpec { text } }

// Warp Gate Spec
object_spec!(
  WarpGateSpec {name: &'static str},
  space_object(3.0, false, Visuals::sprite(MySprite::GPT4O_GATE), name),
  (InteractImpl::WARP_GATE,
   WarpGate { name }));

// Helper constructor for WarpGateSpec
pub const fn warp_gate(name: &'static str) -> WarpGateSpec { WarpGateSpec { name } }

object_spec!(
  SunSpec {}, // No fields needed
  space_object(600.0, false, Visuals::material_sphere(MyMaterial::GLOWY_2), "sun"),
  (CubemapVisibleEntities::default(), CubemapFrusta::default(), PointLight {
    intensity: 3_000_000.0,
    radius: 1.0,
    range: 10000.0,
    shadows_enabled: true,
    color: Color::srgb(0.9, 0.8, 0.6),
    ..default()
  })
);
// No constructor needed, just use SunSpec

// Asteroid Spec (Fixed parameters, but uses a function for scale)
object_spec!(
  AsteroidSpec {},
  space_object(
    asteroid_scale(),
    false,
    Visuals::sprite(MySprite::GPT4O_ASTEROID),
    "Asteroid"
  ),
  (
    InteractImpl::ASTEROID_MINING,
    AsteroidMining { resources: 5, durability: 5 }, // Fixed mining values
    CanBeFollowedByNPC // Keep if necessary, maybe for mining drones?)
  )
);
object_spec!(
  CrystalMonsterSpec {},
  space_object(2.1, true, Visuals::sprite(MySprite::CRYSTALMONSTER), "crystal monster"),
  (
    Combat { hp: 100, is_hostile: true, ..default() },
    NPC { faction: Faction::SPACE_PIRATES, ..default() },
    Navigation::speed(NORMAL_NPC_SPEED * 1.2)
  )
);
object_spec!(
  PlayerSpecDef {},
  space_object(
    PLAYER_SCALE, // Use const
    true,
    Visuals::sprite(MySprite::IMAGEN3WHITESPACESHIP),
    "You"
  ),
  (
    Player::default(),
    Combat { hp: 400, is_hostile: false, ..default() }, // Player stats
    Inventory::default(),
    Navigation::new(PLAYER_FORCE), // Use const
    CanBeFollowedByNPC
  )
);

// Const reference to the player spec instance (as in original code)
// Using the unit struct value directly is often simpler unless you specifically need a trait object.
// Let's provide both options:
pub const PLAYER_SPEC_INSTANCE: PlayerSpecDef = PlayerSpecDef {}; // Direct instance
pub const PLAYER_SPEC: &dyn ObjectSpec = &PLAYER_SPEC_INSTANCE; // Trait object reference

// --- Example Usage (similar to Object::spawn_at) ---

// You would now use the Spec structs directly, perhaps with a helper function:
pub fn spawn_spec_at(c: &mut Commands, spec: &impl ObjectSpec, pos: Vec3) -> Entity {
  let mut ec = c.spawn(Transform::from_translation(pos));
  spec.insert(&mut ec);
  ec.id()
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
  InteractableObject {
    name: &'static str,
    scale: f32,
    can_move: bool,
    visuals: Visuals,
    interact: InteractImpl
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
  // SpaceCowboy,
  SpacePirate,
  SpacePirateBase,
  SpaceStation,
  SpaceWizard,
  Spaceman,
  // SphericalCow,
  TradeStation,
  Trader,
  TreasureContainer,
  Wormhole
}
impl Object {
  // pub const ARRAKIS:Self = Self::Planet { planet_type: (), radius: (), population: (), name: () }
  pub fn spawn_at(self, c: &mut Commands, pos: Vec3) -> Entity {
    let mut ec = c.spawn(Transform::from_translation(pos));
    self.insert(())(&mut ec);
    // Object::insert(&mut ec, self, ());
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
  fn insert(self, b: impl Bundle) -> Box<dyn FnOnce(&mut EntityCommands)> {
    Box::new(move |m: &mut EntityCommands| {
      m.insert(b);
      (match self {
        Self::Empty => {
          // Base case: Empty does nothing further
          Box::new(move |_m: &mut EntityCommands| {})
        }
        Self::SpaceObject { scale, can_move, visuals, name } => {
          let collider = Collider::sphere(1.0);
          Self::Empty.insert((
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
          ))
        }
        Self::NPC { name, hp, speed, sprite, scale, faction } => {
          Self::space_object(scale, true, Visuals::sprite(sprite), name).insert((
            Navigation::speed(speed),
            NPC { follow_target: None, faction },
            Combat { hp, is_hostile: false, ..default() }
          ))
        }
        Self::Planet { sprite, radius, population, name } => {
          Self::space_object(radius, false, Visuals::sprite(sprite), name)
            .insert((Planet { population },))
        }
        Self::TalkingPerson { name, sprite, dialogue_tree } => {
          // Construct the base SpaceObject, then delegate
          Self::space_object(1.7, true, Visuals::sprite(sprite), name).insert((
            DialogueTreeInteract::new(dialogue_tree),
            InteractImpl::DIALOGUE_TREE // Interact::dialogue_tree_default_state(dialogue_tree),
                                        // Maybe add NPC components too if they can move/interact beyond talking?
                                        // Navigation::speed(NORMAL_NPC_SPEED * 0.5), // Example: slow speed
                                        // NPC { faction: Faction::TRADERS, ..default() }, // Example faction
          ))
        }
        Self::ScaledNPC { scale, name, speed, faction, hp, sprite } => {
          Self::space_object(scale, true, Visuals::sprite(sprite), name).insert((
            Navigation::speed(speed),
            NPC { follow_target: None, faction },
            Combat { hp, is_hostile: false, ..default() } // Default non-hostile
          ))
        }
        Self::Enemy { name, hp, speed, sprite } => {
          Self::space_object(NORMAL_NPC_SCALE, true, Visuals::sprite(sprite), name).insert((
            Navigation::new(speed),
            NPC { follow_target: None, faction: Faction::SPACE_PIRATES },
            Combat { hp, is_hostile: true, ..default() } // Explicitly hostile
          ))
        }
        Self::LootObject { sprite, scale, name, item_type } => {
          Self::space_object(scale, true, Visuals::sprite(sprite), name)
            .insert((Container(vec![(item_type, 1)]), InteractImpl::CONTAINER))
        }
        Self::TreasureContainer => Self::space_object(
          2.1,
          true,
          Visuals::sprite(MySprite::GPT4O_CONTAINER),
          "container"
        )
        .insert((
          InteractImpl::CONTAINER,
          Container(vec![(Item::SPACECOIN, 4), (Item::COFFEE, 1)])
        )),
        Self::Explorer => Self::NPC {
          name: "explorer",
          hp: 50,
          speed: NORMAL_NPC_SPEED,
          sprite: MySprite::GPT4O_WHITE_EXPLORATION_SHIP,
          scale: NORMAL_NPC_SCALE,
          faction: Faction::EXPLORERS
        }
        .insert(()),
        Self::Trader => Self::ScaledNPC {
          scale: NORMAL_NPC_SCALE,
          name: "Trader",
          speed: NORMAL_NPC_SPEED,
          faction: Faction::TRADERS,
          hp: 30,
          sprite: MySprite::SPACESHIPWHITE2
        }
        .insert(()),
        Self::SpaceCop => Self::ScaledNPC {
          scale: NORMAL_NPC_SCALE,
          name: "space cop",
          speed: NORMAL_NPC_SPEED,
          faction: Faction::SPACE_POLICE,
          hp: 70,
          sprite: MySprite::GPT4O_POLICE_SPACE_SHIP
        }
        .insert(()),
        Self::SpaceWizard => Self::ScaledNPC {
          scale: NORMAL_NPC_SCALE,
          name: "space wizard",
          speed: NORMAL_NPC_SPEED,
          faction: Faction::SPACEWIZARDS,
          hp: 40,
          sprite: MySprite::IMAGEN3WIZARDSPACESHIP
        }
        .insert(()),
        Self::Miner => Self::ScaledNPC {
          scale: NORMAL_NPC_SCALE,
          name: "miner",
          speed: NORMAL_NPC_SPEED,
          faction: Faction::TRADERS,
          hp: 35,
          sprite: MySprite::GPT4O_MINING_SHIP
        }
        .insert(()),
        Self::AlienSoldier => Self::ScaledNPC {
          scale: NORMAL_NPC_SCALE,
          name: "alien soldier",
          speed: NORMAL_NPC_SPEED,
          faction: Faction::INVADERS,
          hp: 80,
          sprite: MySprite::PURPLEENEMYSHIP
        }
        .insert(()),
        Self::MinerNpc => Self::NPC {
          name: "miner npc",
          hp: 45,
          speed: NORMAL_NPC_SPEED * 0.8,
          sprite: MySprite::GPT4O_MINING_SHIP,
          scale: NORMAL_NPC_SCALE,
          faction: Faction::TRADERS
        }
        .insert(()),
        Self::MushroomMan => Self::ScaledNPC {
          scale: NORMAL_NPC_SCALE,
          name: "mushroom man",
          speed: NORMAL_NPC_SPEED * 0.9,
          faction: Faction::TRADERS, // ?? Seems odd, maybe needs own faction
          hp: 40,
          sprite: MySprite::MUSHROOMMAN
        }
        .insert(()),
        // Self::SpaceCowboy => Self::TalkingPerson {
        //   name: "space cowboy",
        //   sprite: MySprite::SPACECOWBOY,
        //   dialogue_tree: SPACE_COWBOY_DIALOGUE
        // }
        // .insert(()),
        Self::Sign { text } => Self::space_object(
          1.5,
          false, // Static
          Visuals::sprite(MySprite::GPT4O_SIGN),
          "sign"
        )
        .insert((InteractImpl::DESCRIBE, TextDisplay(text.to_string()))),
        Self::InteractableObject { name, scale, can_move, visuals, interact } => {
          Self::space_object(scale, can_move, visuals, name).insert(interact)
        }
        Self::Wormhole => {
          Self::space_object(4.0, false, Visuals::sprite(MySprite::WORMHOLE), "wormhole")
            .insert((InteractImpl::DESCRIBE, TextDisplay("this is a wormhole".to_string())))
        }
        Self::Asteroid => Self::space_object(
          asteroid_scale(),
          false, // Typically static unless destructible/mineable changes state
          Visuals::sprite(MySprite::GPT4O_ASTEROID),
          "Asteroid"
        )
        .insert((
          InteractImpl::ASTEROID_MINING,
          AsteroidMining { resources: 5, durability: 5 },
          CanBeFollowedByNPC // Why would NPCs follow an asteroid? Maybe for mining?
        )),
        Self::SpaceCat => {
          Self::loot_object(1.3, "space cat", MySprite::GPT4O_SPACE_CAT, Item::SPACECAT)
            .insert(())
        }
        Self::Spaceman => {
          Self::loot_object(1.3, "spaceman", MySprite::GPT4O_SPACE_MAN, Item::PERSON)
            .insert(())
        }
        Self::SpaceCoin => {
          Self::loot_object(1.7, "space coin", MySprite::COIN, Item::SPACECOIN).insert(())
        }
        Self::IceAsteroid => Self::loot_object(
          asteroid_scale(),
          "ice asteroid",
          MySprite::GPT4O_ICE_ASTEROID,
          Item::DIHYDROGENMONOXIDE
        )
        .insert(()), // Should probably have Interact::Item or similar
        Self::CrystalAsteroid => Self::loot_object(
          asteroid_scale(),
          "crystal asteroid",
          MySprite::CRYSTALASTEROID,
          Item::CRYSTAL
        )
        .insert(()), // Should probably have Interact::Item or similar
        Self::CrystalMonster => Self::space_object(
          2.1,
          true,
          Visuals::sprite(MySprite::CRYSTALMONSTER),
          "crystal monster"
        )
        .insert((
          Combat { hp: 100, is_hostile: true, ..default() },
          NPC { faction: Faction::SPACE_PIRATES, ..default() }, // Monsters faction?
          Navigation::speed(NORMAL_NPC_SPEED * 1.2)
        )),
        Self::CrystalMonster2 => Self::space_object(
          1.7,
          true,                                      // Assuming it moves
          Visuals::sprite(MySprite::CRYSTALMONSTER), // Same sprite?
          "lesser crystal monster"
        )
        .insert((
          InteractImpl::DESCRIBE,
          TextDisplay("it's a lesser crystal monster".to_string())
        )),
        Self::HpBox => {
          Self::space_object(0.9, true, Visuals::sprite(MySprite::HPBOX), "hp box")
            .insert(InteractImpl::HP_BOX)
        }
        // Self::SphericalCow => Self::TalkingPerson {
        //   name: "spherical cow",
        //   sprite: MySprite::GPT4O_SPHERICAL_COW,
        //   dialogue_tree: SPHERICAL_SPACE_COW_DIALOGUE
        // }
        // .insert(()),
        Self::TradeStation => {
          let (trade_interaction, text) = if prob(0.5) {
            let &trade_buy =
              pick(&[Item::DIHYDROGENMONOXIDE, Item::CRYSTAL, Item::SPACECAT]).unwrap();
            (
              TradeInteract { inputs: (trade_buy, 1), outputs: (Item::SPACECOIN, 5) },
              format!("space station\nbuys {:?}", trade_buy)
            )
          } else {
            let &trade_sell = pick(&[Item::SPICE, Item::COFFEE, Item::ROCK]).unwrap();
            (
              TradeInteract { inputs: (Item::SPACECOIN, 5), outputs: (trade_sell, 1) },
              format!("space station\nsells {:?}", trade_sell)
            )
          };
          Self::space_object(
            3.0,
            false, // Static
            Visuals::sprite(MySprite::IMAGEN3SPACESTATION),
            "space station"
          )
          .insert((
            InteractImpl::TRADE,
            CanBeFollowedByNPC, // NPCs can dock/interact?
            trade_interaction,
            TextDisplay::from(text)
          ))
        }
        Self::FloatingIsland => Self::space_object(
          3.4,
          false, // Static
          Visuals::sprite(MySprite::FLOATINGISLAND),
          "floating island"
        )
        .insert((
          InteractImpl::DESCRIBE,
          TextDisplay("it's a floating island in space".to_string())
        )),
        Self::Sun => {
          Self::space_object(
            600.0,
            false, // Static
            Visuals::material_sphere(MyMaterial::GLOWY_2),
            "sun"
          )
          .insert((
            // Components for lighting
            CubemapVisibleEntities::default(),
            CubemapFrusta::default(),
            PointLight {
              intensity: 3_000_000.0, // Very intense
              radius: 1.0,            // Light source size (affects shadow softness)
              range: 10000.0,         // How far light reaches
              shadows_enabled: true,
              color: Color::srgb(0.9, 0.8, 0.6), // Yellowish
              ..default()
            }
          ))
        }
        Self::AbandonedShip => {
          Self::space_object(
            2.0,
            false, // Static derelict
            Visuals::sprite(MySprite::SPACESHIPABANDONED),
            "abandoned ship"
          )
          .insert((InteractImpl::SALVAGE, Salvage { loot: 5 }))
        }
        Self::Player => Self::space_object(
          PLAYER_SCALE,
          true, // Dynamic
          Visuals::sprite(MySprite::IMAGEN3WHITESPACESHIP),
          "You"
        )
        .insert((
          Player::default(),
          Combat { hp: 400, is_hostile: false, ..default() },
          Inventory::default(),
          Navigation::new(PLAYER_FORCE),
          CanBeFollowedByNPC
        )),
        Self::SpacePirate => Self::Enemy {
          name: "Space Pirate",
          hp: 60,
          speed: NORMAL_NPC_SPEED * 1.1,
          sprite: MySprite::PURPLEENEMYSHIP
        }
        .insert(()),
        Self::SpacePirateBase => Self::space_object(
          4.0,
          false, // Static base
          Visuals::sprite(MySprite::GPT4O_PIRATE_STATION),
          "space pirate base"
        )
        .insert((
          Combat { hp: 120, is_hostile: false, ..default() },
          InteractImpl::DESCRIBE,
          TextDisplay("it's a space pirate base".to_string())
        )),
        Self::SpaceStation => Self::space_object(
          4.0,
          false, // Static base
          Visuals::sprite(MySprite::GPT4O_TRADING_STATION),
          "space station"
        )
        .insert((
          Combat { hp: 120, is_hostile: false, ..default() }, // Non-hostile
          InteractImpl::DESCRIBE,
          TextDisplay("it's a space station".to_string())
        )),
        Self::Nomad => Self::npc(
          NORMAL_NPC_SCALE,
          "nomad",
          NORMAL_NPC_SPEED,
          Faction::WANDERERS,
          35,
          MySprite::GPT4O_GREEN_CAR_SHIP
        )
        .insert(()),
        Self::WarpGate { name } => {
          Self::space_object(3.0, false, Visuals::sprite(MySprite::GPT4O_GATE), name)
            .insert((InteractImpl::WARP_GATE, WarpGate { name }))
        }
      })(m); // Execute the returned closure
    })
  }
}

pub fn from<B, A: From<B>>(b: B) -> A { A::from(b) }

fn rangerand(lo: f32, hi: f32) -> f32 { lo.lerp(hi, rand::random::<f32>()) }
fn random_zone_name() -> String {
  String::from_utf8((0..4).map(|_| rangerand(0.0, 30.0) as u8).collect()).unwrap()
  // (0..4).map(|_| random::<char>()).collect()
}
fn asteroid_scale() -> f32 { rangerand(1.8, 5.3) }
fn random_normalized_vector() -> Vec3 { random::<Quat>() * Vec3::X }
fn prob(p: f32) -> bool { p > random::<f32>() }

#[derive(Component, Clone)]
struct ZoneEntity {
  zone: Entity
}

const MAX_ZONE_RANGE: f32 = 200.0;

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

pub type Zone = &'static [([f32; 3], Object)];
pub type WorldLayout = &'static [([f32; 3], Zone)];
fn spawn_world_layout(world_layout: WorldLayout, mut commands: &mut Commands) {
  for &(zone_pos, zone) in world_layout {
    for &(object_pos, object) in zone {
      object.spawn_at(commands, Vec3::from_array(zone_pos) + Vec3::from_array(object_pos));
    }
  }
}
pub fn setup(
  playerq: Query<&Transform, With<Player>>,
  serv: Res<AssetServer>,
  mut meshes: ResMut<Assets<Mesh>>,
  mut materials: ResMut<Assets<StandardMaterial>>,
  mut space_shader_materials: ResMut<Assets<SpaceShaderMaterial>>,
  mut c: Commands
) {
  // c.spawn(GenMesh::SPHERE.generate().transformed_by(transform))

  c.spawn((
    Mesh3d(meshes.add(Cuboid::new(3000.0, 1000.0, 1000.0))),
    MeshMaterial3d(space_shader_materials.add(SpaceShaderMaterial {})),
    Transform::from_xyz(0.0, 0.0, 0.0)
  ));
  spawn_world_layout(aigen::solar_system::WORLD_LAYOUT, &mut c);

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
      pitch_upper_limit: Some(pitch_limit_radians),
      pitch_lower_limit: Some(-pitch_limit_radians),
      zoom_upper_limit: Some(200.0),
      zoom_lower_limit: 5.0,
      orbit_smoothness: 0.0,
      pan_sensitivity: 0.0,
      pan_smoothness: 0.7,
      zoom_sensitivity: 2.5,
      ..default()
    }
  );
  c.spawn(camera);
  println("spawned camera");
  println("setup");
}

// #[derive(Clone)]
// struct SpaceShaderMaterial;

#[derive(Asset, TypePath, AsBindGroup, Debug, Clone)]
struct SpaceShaderMaterial {}
impl Material for SpaceShaderMaterial {
  fn fragment_shader() -> ShaderRef {
    "space_shader.wgsl".into() // Make sure this path is correct
  }
  fn vertex_shader() -> ShaderRef {
    "space_shader.wgsl".into() // Make sure this path is correct
  }
  fn alpha_mode(&self) -> AlphaMode { AlphaMode::Opaque }

  // *** Implement specialize to fix culling and depth ***
  fn specialize(
    _pipeline: &MaterialPipeline<Self>,
    descriptor: &mut RenderPipelineDescriptor,
    _layout: &MeshVertexBufferLayoutRef, // Use the Ref type from the trait source
    _key: MaterialPipelineKey<Self>
  ) -> Result<(), SpecializedMeshPipelineError> {
    // Use the correct error type

    // --- 1. Fix Culling ---
    // We are inside the cube, looking out. The cube's front faces point outward.
    // We need to see the inward-pointing faces (the back faces).
    // We tell the GPU to cull the *front* faces instead of the back faces.
    descriptor.primitive.cull_mode = Some(Face::Front);
    // Alternatively, disable culling completely:
    // descriptor.primitive.cull_mode = None; // Usually 'Front' is slightly better

    // --- 2. Set Correct Depth State ---
    // We want the skybox to draw only where nothing closer has been drawn.
    // The vertex shader sets depth to 1.0 (far plane).
    if let Some(depth_stencil) = descriptor.depth_stencil.as_mut() {
      // Use LessEqual comparison: Draw if the incoming fragment depth (1.0)
      // is Less than or Equal to the existing depth in the buffer.
      // This ensures it draws over the clear depth (1.0) but gets correctly
      // overwritten by any scene objects closer than the far plane.
      depth_stencil.depth_compare = CompareFunction::LessEqual;

      // IMPORTANT: Keep depth writes ENABLED. If the skybox pixel passes the
      // test (i.e., in empty space), it should write 1.0 to the depth buffer.
      // Disabling writes can cause issues where objects drawn *after* the
      // skybox might incorrectly render "behind" it if they happen to fall
      // in pixels where the skybox should have been but didn't write depth.
      depth_stencil.depth_write_enabled = true;
    }

    Ok(())
  }
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
      MaterialPlugin::<SpaceShaderMaterial>::default(),
      // bevy_vox_scene::VoxScenePlugin,
      bevy_sprite3d::Sprite3dPlugin,
      bevy_panorbit_camera::PanOrbitCameraPlugin,
      // bevy_mod_billboard::prelude::BillboardPlugin,
      avian3d::PhysicsPlugins::default()
    ))
    .init_resource::<TimeTicks>()
    .insert_resource(gravity)
    .insert_resource(solver_config)
    .insert_resource(ClearColor(Color::BLACK))
    .insert_resource(AMBIENT_LIGHT)
    .add_systems(Startup, (setup, setup_ui_system).chain())
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

// trunk build --release --minify --public-url "bevyspacegame" --filehash false

// trunk serve

// cargo check --target wasm32-unknown-unknown
// cargo run --target x86_64-unknown-linux-gnu
// cargo check --target x86_64-unknown-linux-gnu
