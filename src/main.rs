#![allow(clippy::unnecessary_cast)]
#![allow(unused_imports)]
#![allow(dead_code)]
#![feature(const_trait_impl)]
// #![feature(type_alias_impl_trait)]
#![allow(unused_mut)]
#![allow(non_camel_case_types)]
#![feature(variant_count)]
#![feature(strict_overflow_ops)]
#![feature(iter_intersperse)]
#![feature(trivial_bounds)]
#![feature(impl_trait_in_assoc_type)]
#![feature(option_get_or_insert_default)]
#![feature(let_chains)]
#![feature(const_mut_refs)]

// #![feature(int_roundings)]
// #![recursion_limit = "1024"]
// #![feature(const_fn_floating_point_arithmetic)]

pub mod bundletree;
pub mod ui;

use std::{cell::OnceCell, iter::Enumerate, sync::{Arc, LazyLock}};

use lazy_static::lazy_static;
pub use bevy::prelude::Name;
use {avian3d::prelude::*,
     bevy::{app::AppExit,
            asset::{AssetServer, Handle},
            core_pipeline::{bloom::{BloomCompositeMode, BloomPrefilterSettings,
                                    BloomSettings},
                            Skybox},
            ecs::{entity::EntityHashMap, world::Command},
            math::{primitives, vec3, Vec3},
            pbr::{CubemapVisibleEntities, StandardMaterial},
            prelude::*,
            render::{primitives::CubemapFrusta,
                     render_resource::{AsBindGroupShaderType, TextureViewDescriptor},
                     texture::{ImageAddressMode, ImageFilterMode, ImageSamplerDescriptor}},
            utils::{HashMap, HashSet},
            window::WindowMode},
     bevy_embedded_assets::*,
     bevy_mod_billboard::{BillboardDepth, BillboardLockAxis, BillboardMeshHandle,
                          BillboardTextBundle, BillboardTextureBundle,
                          BillboardTextureHandle},
     bevy_mod_picking::{prelude::{Highlight, HighlightKind},
                        PickableBundle},
     bevy_panorbit_camera::PanOrbitCamera,
     bevy_quill::{prelude::*, QuillPlugin},
     bevy_quill_overlays::QuillOverlaysPlugin,
     dynamics::solver::SolverConfig,
     enum_assoc::Assoc,
     fancy_constructor::new,
     rand::{random, thread_rng, Rng},
     rust_utils::{comment, debug_println, debugfmt, filter_map, find, find_map, first,
                  map, mapv, prettyfmt, println, sort_by_key, sum, vec},
     std::{any::Any, cell::LazyCell, f32::consts::PI},
     ui::{intersperse_newline, ui_root_thing_in_the_world, Message, UIData, UIMainView}};

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
pub const AMBIENT_LIGHT: AmbientLight = AmbientLight { color: Color::WHITE,
                                                       brightness: 300.0 };
pub const BLOOM_SETTINGS: BloomSettings =
  BloomSettings { intensity: 0.5,
                  low_frequency_boost: 0.0,
                  prefilter_settings: BloomPrefilterSettings { threshold: 2.2,
                                                               threshold_softness: 0.0 },
                  composite_mode: BloomCompositeMode::Additive,
                  ..BloomSettings::NATURAL };

#[derive(Assoc, Copy, Clone, Hash, Eq, PartialEq)]
#[func(pub const fn path(&self) -> &'static str)]
pub enum MySprite {
  #[assoc(path = "white_corners.png")]
  WhiteCorners,
  #[assoc(path = "spaceman.png")]
  SpaceMan,
  #[assoc(path = "spacecowboy.png")]
  SpaceCowBoy,
  #[assoc(path = "spacewizard.png")]
  SpaceWizard,
  #[assoc(path = "wormhole.png")]
  WormHole,
  #[assoc(path = "gate.png")]
  Gate,
  #[assoc(path = "turret.png")]
  Turret,
  #[assoc(path = "crystal_monster.png")]
  CrystalMonster,
  #[assoc(path = "container.png")]
  Container,
  #[assoc(path = "mushroom_man.png")]
  MushroomMan,
  #[assoc(path = "asteroid.png")]
  Asteroid,
  #[assoc(path = "icesteroid.png")]
  IceAsteroid,
  #[assoc(path = "crystal_asteroid.png")]
  CrystalAsteroid,
  #[assoc(path = "coin.png")]
  Coin,
  #[assoc(path = "space_cat.png")]
  SpaceCat,
  #[assoc(path = "spherical_cow.png")]
  SphericalCow,
  #[assoc(path = "zorp.png")]
  Zorp,
  #[assoc(path = "space_station.png")]
  SpaceStation,
  #[assoc(path = "ice_planet.png")]
  IcePlanet,
  #[assoc(path = "lava_planet.png")]
  LavaPlanet,
  #[assoc(path = "pixelc/habitableplanet.png")]
  HabitablePlanet,
  #[assoc(path = "pixelc/browngasgiant.png")]
  BrownGasGiant,
  #[assoc(path = "pixelc/marslikeplanet.png")]
  MarsLikePlanet,
  #[assoc(path = "sandplanet.png")]
  SandPlanet,
  #[assoc(path = "hpbox.png")]
  HPBox,
  #[assoc(path = "sign.png")]
  Sign,
  #[assoc(path = "floating_island.png")]
  FloatingIsland,
  #[assoc(path = "spacepiratebase.png")]
  SpacePirateBase,
  #[assoc(path = "spaceshipwhite.png")]
  SpaceshipWhite,
  #[assoc(path = "spaceshipblue.png")]
  SpaceshipBlue,
  #[assoc(path = "spaceshipred.png")]
  SpaceshipRed,
  #[assoc(path = "spaceshipdarkred.png")]
  SpaceshipDarkRed,
  #[assoc(path = "spaceshippurple.png")]
  SpaceshipPurple,
  #[assoc(path = "spaceshipabandoned.png")]
  SpaceshipAbandoned,
  #[assoc(path = "wizardspaceship.png")]
  WizardSpaceShip,
  #[assoc(path = "spaceshipgreen.png")]
  SpaceshipGreen,
  #[assoc(path = "purpleenemyship.png")]
  PurpleEnemyShip,
  #[assoc(path = "spaceshipwhite2.png")]
  SpaceshipWhite2,
  #[assoc(path = "stone.png")]
  Stone,
  #[assoc(path = "pixelc/bricks.png")]
  Bricks,
  #[assoc(path = "pixelc/chest.png")]
  Chest,
  #[assoc(path = "pixelc/block_textures.png")]
  BlockTextures,
  #[assoc(path = "sun.png")]
  Sun,
  #[assoc(path = "fire.png")]
  Fire,
  #[assoc(path = "iceberg.png")]
  Iceberg,
  #[assoc(path = "coffee.png")]
  Coffee,
  #[assoc(path = "stickman.png")]
  Stickman,
  #[assoc(path = "grass.png")]
  Grass,
  #[assoc(path = "water.png")]
  Water,
  #[assoc(path = "tree.png")]
  Tree,
  #[assoc(path = "snow.png")]
  Snow,
  #[assoc(path = "penguin.png")]
  Penguin,
  #[assoc(path = "pixelc/missile.png")]
  Missile,
  #[assoc(path = "pixelc/torch.png")]
  Torch,
  #[assoc(path = "nasa_starmap.jpeg")]
  NasaStarmap
}
#[derive(Assoc, Copy, Clone, Hash, Eq, PartialEq)]
#[func(pub fn val(&self, h: Handle<Image>) -> StandardMaterial)]
#[func(pub fn img(&self) -> MySprite)]
enum MyImageMaterial {
  #[assoc(img = MySprite::Snow)]
  #[assoc(val = StandardMaterial { perceptual_roughness: 0.4,
                                   metallic: 0.0,
                                   reflectance: 0.5,
                                   ior: 1.31,
                                   base_color_texture: Some(h),
                                   ..default() })]
  Snow,
  #[assoc(img = MySprite::Water)]
  #[assoc(val = StandardMaterial { perceptual_roughness: 0.3,
                                                      metallic: 0.0,
                                                      reflectance: 0.5,
                                                      base_color_texture:
                                                      Some(h),
                                                      ..default() })]
  Water,
  #[assoc(img = MySprite::Stone)]
  #[assoc(val = StandardMaterial { perceptual_roughness: 0.8,
                                                      metallic: 0.0,
                                                      reflectance: 0.3,
                                                      base_color_texture:
                                                      Some(h),
                                                      ..default() })]
  Stone,
  #[assoc(img = MySprite::Bricks)]
  #[assoc(val = StandardMaterial { perceptual_roughness: 0.95,
                                                      metallic: 0.0,
                                                      reflectance: 0.1,
                                                      base_color_texture:
                                   Some(h),
                                                      ..default() })]
  Bricks,
  #[assoc(img = MySprite::Grass)]
  #[assoc(val = StandardMaterial { perceptual_roughness: 0.8,
                                                      metallic: 0.0,
                                                      reflectance: 0.2,
                                                      base_color_texture:
                                   Some(h),
                                                      ..default() })]
  Grass,
  #[assoc(img = MySprite::Penguin)]
  #[assoc(val = StandardMaterial::from(h))]
  Penguin
}
#[derive(Assoc, Copy, Clone, Hash, Eq, PartialEq)]
#[func(pub fn val(&self) -> StandardMaterial)]
pub enum MyMaterial {
  #[assoc(val = StandardMaterial { unlit: true,
                                   alpha_mode: AlphaMode::Mask(0.0),
                                   ..GLOWY_COLOR.into() })]
  GlowyMaterial,
  #[assoc(val = StandardMaterial { unlit: true,
                                   alpha_mode: AlphaMode::Mask(0.0),
                                   ..GLOWY_COLOR_2.into() })]
  GlowyMaterial2,
  #[assoc(val = StandardMaterial { unlit: true,
                                   alpha_mode: AlphaMode::Mask(0.0),
                                   ..GLOWY_COLOR_3.into() })]
  GlowyMaterial3,
  #[assoc(val = StandardMaterial { unlit: true,
                                   alpha_mode: AlphaMode::Mask(0.0001),
                                   ..EXPLOSION_COLOR.into() })]
  ExplosionMaterial,
  #[assoc(val = StandardMaterial { unlit: true,
                                   alpha_mode: AlphaMode::Mask(0.0001),
                                   ..LASER_COLOR.into() })]
  LaserMaterial,
  #[assoc(val = StandardMaterial::from(Color::srgb(0.2, 0.7, 0.9)))]
  ParticleMaterial,
  #[assoc(val = StandardMaterial { unlit: true,
                                   alpha_mode: AlphaMode::Blend,
                                   ..Color::srgba(0.0, 0.0, 0.0, 0.0).into() })]
  InvisibleMaterial,
  #[assoc(val = StandardMaterial { unlit: true,
                                   alpha_mode: AlphaMode::Blend,
                                   ..Color::srgba(0.0, 0.3, 1.0, 0.1).into() })]
  HoveredMaterial,
  #[assoc(val = StandardMaterial { unlit: true,
                                   alpha_mode: AlphaMode::Blend,
                                   ..Color::srgba(0.0, 0.3, 1.0, 0.3).into() })]
  PressedMaterial,
  #[assoc(val = StandardMaterial { unlit: true,
                                   alpha_mode: AlphaMode::Blend,
                                   ..Color::srgba(0.0, 0.3, 1.0, 0.2).into() })]
  SelectedMaterial
}
#[derive(Assoc, Copy, Clone, Hash, Eq, PartialEq)]
#[func(pub const fn path_and_label(&self) -> (&'static str,&'static str))]
pub enum MyScene {
  #[assoc(path_and_label = ("lunarlander.glb", "Scene0"))]
  LunarLander,
  #[assoc(path_and_label = ("character_controller_demo.glb", "Scene0"))]
  CharacterControllerDemo,
  #[assoc(path_and_label = ("level.glb", "Scene0"))]
  Level,
  #[assoc(path_and_label = ("alevel.gltf", "Scene0"))]
  ALevel,
  #[assoc(path_and_label = ("this_here_level.glb", "Scene0"))]
  IslandLevel,
  #[assoc(path_and_label = ("somesketchlevel.glb", "Scene0"))]
  SomeSketchLevel,
  #[assoc(path_and_label = ("snowman.glb", "Scene0"))]
  Snowman,
  #[assoc(path_and_label = ("coffee.glb", "Scene0"))]
  CoffeeScene,
  #[assoc(path_and_label = ("goxel_level.glb", "Scene0"))]
  GoxelLevel,
  #[assoc(path_and_label = ("turtle level.gltf", "Scene0"))]
  TurtleLevel,
  #[assoc(path_and_label = ("wat.glb", "Scene0"))]
  Wat
}
// struct GenMesh2(pub fn() -> Mesh);
// const CUBE: GenMesh2 = GenMesh2(|| Cuboid::new(0.7, 0.7, 0.7).into());
#[derive(Assoc, Copy, Clone, Hash, Eq, PartialEq)]
#[func(pub fn gen(&self) -> Mesh)]
pub enum GenMesh {
  #[assoc(gen = Cuboid::new(1.0, 1.0, 1.0).into())]
  UnitCube,
  #[assoc(gen = primitives::Cylinder::new(1.0, 1.0).into())]
  UnitCylinder,
  #[assoc(gen = Cuboid::new(0.7, 0.7, 0.7).into())]
  Cube,
  #[assoc(gen = Cuboid::new(2.0, 1.0, 1.0).into())]
  BoxMesh,
  #[assoc(gen = Cuboid::new(2.1, 0.3, 2.1).into())]
  FlatBox,
  #[assoc(gen = primitives::Capsule3d::default().into())]
  Capsule,
  #[assoc(gen = primitives::Torus::default().into())]
  Torus,
  #[assoc(gen = primitives::Sphere { radius: 1.0 }.into())]
  Sphere,
  #[assoc(gen = Cuboid::new(25.0, 0.1, 25.0).into())]
  PlaneSize50,
  #[assoc(gen = primitives::Rectangle::new(BILLBOARD_REL_SCALE, BILLBOARD_REL_SCALE).into())]
  BillboardMeshSquare
}

#[derive(Component, Clone, PartialEq, Eq, Default)]
pub struct Visuals {
  text: Option<String>,
  material_mesh: Option<(MyMaterial, GenMesh)>,
  shield_active: bool,
  sprite: Option<MySprite>,
  targeted: bool
}

impl Visuals {
  fn none() -> Self { default() }
  fn sprite(sprite: MySprite) -> Self {
    Self { sprite: Some(sprite),
           ..default() }
  }
  fn material_mesh(material: MyMaterial, mesh: GenMesh) -> Self {
    Self { material_mesh: Some((material, mesh)),
           ..default() }
  }
  fn material_sphere(material: MyMaterial) -> Self {
    Self::material_mesh(material, GenMesh::Sphere)
  }
  fn with_text(self, text: impl ToString) -> Self {
    Self { text: Some(text.to_string()),
           ..self }
  }
}
pub fn set_visuals(mut visuals_q: Query<(Entity, &mut Visuals, Option<&Combat>)>,
                   mut player_q: Query<&Player>) {
  if let Ok(player) = player_q.get_single() {
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
pub fn visuals(camq: Query<&GlobalTransform, With<Camera3d>>,
               serv: Res<AssetServer>,
               mut c: Commands,
               mut n: Local<u32>,
               mut visuals_q: Query<(Entity, Ref<Visuals>)>,
               mut visuals_sprites_q: Query<(&mut Transform, &GlobalTransform),
                     With<VisualSprite>>,
               mut option_target_overlay_entity: Local<Option<Entity>>,
               mut sprite_handles: Local<HashMap<MySprite, Handle<Image>>>,
               mut mesh_handles: Local<HashMap<GenMesh, Handle<Mesh>>>,
               mut material_handles: Local<HashMap<MyMaterial, Handle<StandardMaterial>>>,
               mut visual_child_entities: Local<EntityHashMap<Entity>>,
               mut multi_visual_child_entities: Local<EntityHashMap<Entity>>) {
  let mut get_material_handle = |material: MyMaterial| {
    material_handles.entry(material)
                    .or_insert_with(|| serv.add(material.val()))
                    .clone()
  };
  let mut get_mesh_handle = |mesh: GenMesh| {
    mesh_handles.entry(mesh)
                .or_insert_with(|| serv.add(mesh.gen()))
                .clone()
  };
  let mut get_sprite_handle = |sprite: MySprite| {
    sprite_handles.entry(sprite)
                  .or_insert_with(|| serv.load(format!("embedded://{}", sprite.path())))
                  .clone()
  };

  let text_style = TextStyle { font_size: 30.0,
                               ..default() };
  let invisible_material = get_material_handle(MyMaterial::InvisibleMaterial);
  let invisible_highlight =
    Highlight { hovered: Some(HighlightKind::Fixed(invisible_material.clone())),
                pressed: Some(HighlightKind::Fixed(invisible_material.clone())),
                selected: Some(HighlightKind::Fixed(invisible_material.clone())) };

  for (e, visuals) in &visuals_q {
    if visuals.is_changed() {
      *n += 1;
      // println(*n);
      if *n % 100 == 0 {
        println(*n);
      }
      let main_visual_child =
        *(visual_child_entities.entry(e).or_insert_with(|| {
                                          c.spawn((PickableBundle::default(),
                                                   invisible_highlight.clone(),
                     PbrBundle { material: invisible_material.clone() ,
                                 mesh: get_mesh_handle(GenMesh::Sphere),
                                 ..default() }))
             .set_parent(e)
             .id()
                                        }));
      c.entity(main_visual_child).despawn_descendants();
      if let Some(text) = visuals.text.clone() {
        c.spawn(BillboardTextBundle { text: Text::from_section(text, text_style.clone()),
                                      text_bounds: default(),
                                      text_anchor: default(),
                                      transform: Transform::from_xyz(0.0, 1.5, 0.0).with_scale(Vec3::splat(0.07)),
                                      billboard_depth: BillboardDepth(true),
                                      ..default() })
         .set_parent(main_visual_child);
      }
      if let Some(sprite) = visuals.sprite {
        let sprite_handle = get_sprite_handle(sprite);
        let billboard_mesh_handle = get_mesh_handle(GenMesh::BillboardMeshSquare);
        c.spawn((VisualSprite,
                 BillboardLockAxis { y_axis: true,
                                     rotation: true },
                 BillboardTextureBundle { mesh:
                                            BillboardMeshHandle(billboard_mesh_handle),
                                          texture:
                                            BillboardTextureHandle(sprite_handle),
                                          billboard_depth: BillboardDepth(true),
                                          ..default() }))
         .set_parent(main_visual_child);
      }
      if visuals.targeted {
        let target_overlay = get_sprite_handle(MySprite::WhiteCorners);
        let billboard_mesh_handle = get_mesh_handle(GenMesh::BillboardMeshSquare);
        c.spawn((BillboardLockAxis { y_axis: false,
                                     rotation: false },
                 BillboardTextureBundle { mesh:
                                            BillboardMeshHandle(billboard_mesh_handle),
                                          texture:
                                            BillboardTextureHandle(target_overlay),
                                          billboard_depth: BillboardDepth(false),
                                          transform:
                                            Transform::from_scale(Vec3::splat(1.7)),
                                          ..default() }))
         .set_parent(main_visual_child);
      }
      if let Some((material, gen_mesh)) = visuals.material_mesh {
        let material = get_material_handle(material);
        let mesh = get_mesh_handle(gen_mesh);
        c.spawn(PbrBundle { material,
                            mesh,
                            ..default() })
         .set_parent(main_visual_child);
      }
    }
  }
  if let Ok(cam_globaltransform) = camq.get_single() {
    for (mut transform, globaltransform) in &mut visuals_sprites_q {
      let dir = (globaltransform.translation()
                 - cam_globaltransform.translation()).normalize_or(Vec3::Y);
      transform.look_to(dir, Vec3::Y);
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
    self.target_interaction_state =
      Some(PlayerTargetInteractionState { target,
                                          shooting: false,
                                          approaching: false,
                                          in_dialogue: false });
  }
  pub fn untarget(&mut self) { self.target_interaction_state = None; }
  pub fn target(&self) -> Option<Entity> {
    self.target_interaction_state
        .as_ref()
        // .clone()
        .map(|&PlayerTargetInteractionState { target, .. }| target)
  }
}

pub fn insert_component<C: Component>(world: &mut World, entity: Entity, component: C) {
  if let Some(mut entity_mut) = world.get_entity_mut(entity) {
    entity_mut.insert(component);
  }
}

pub fn update_component<C: Component + Clone>(world: &mut World,
                                              entity: Entity,
                                              f: impl FnOnce(C) -> C) {
  if let Some(mut entity_mut) = world.get_entity_mut(entity) {
    if let Some(mut component) = entity_mut.get_mut::<C>() {
      let updated = f((*component).clone());
      *component = updated;
    }
  }
}

pub fn mutate_component<C: Component>(world: &mut World,
                                      entity: Entity,
                                      f: impl FnOnce(&mut C)) {
  if let Some(mut entity_mut) = world.get_entity_mut(entity) {
    if let Some(mut component) = entity_mut.get_mut::<C>() {
      f(&mut component);
    }
  }
}

pub fn get_player(world: &mut World) -> Option<Entity> {
  world.query_filtered::<Entity, With<Player>>()
       .iter(world)
       .next()
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
    fn from(f: F) -> Self {
        MyCommand(Box::new(f))
    }
}
impl MyCommand {
  pub fn none() -> Self { (|_world: &mut World| {}).into() }

  pub fn multi(commands: impl IntoIterator<Item = MyCommand>) -> Self {
    let v = vec(commands.clone());
    (move |world: &mut World| {
      for command in v {
        command.0(world);
      }
    }).into()
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
    }).into()
  }

  pub fn end_object_interaction_mini_game() -> Self {
    (|_world: &mut World| {
      // Implement mini-game ending logic here
    }).into()
  }

  pub fn damage_entity(entity: Entity, amount: u32) -> Self {
    (move |world: &mut World| {
      if let Some(mut combat) = world.get_mut::<Combat>(entity) {
        combat.hp = combat.hp.saturating_sub(amount);
      }
    }).into()
  }

  pub fn message_add(message: impl ToString + Send + Sync + 'static) -> Self {
    (move |world: &mut World| {
      if let Some(mut ui_data) = world.get_resource_mut::<UIData>() {
        ui_data.message_add(message.to_string().clone());
      }
    }).into()
  }

  pub fn despawn_entity(entity: Entity) -> Self {
    (move |world: &mut World| {
      world.commands().entity(entity).despawn_recursive();
    }).into()
  }
  pub fn despawn(entity: Entity) -> Self {
    (move |world: &mut World| {
      world.commands().entity(entity).despawn_recursive();
    }).into()
  }

  pub fn insert_component<C: Component + 'static>(entity: Entity, component: C) -> Self {
    (move |world: &mut World| insert_component(world, entity, component)).into()
  }

  pub fn update_component<C: Component + Clone + 'static>(entity: Entity,
                                                          f: impl FnOnce(C) -> C
                                                            + 'static
                                                            + Send
                                                            + Sync)
                                                          -> Self {
    (move |world: &mut World| update_component(world, entity, f)).into()
  }

  pub fn mutate_component<C: Component + 'static>(entity: Entity,
                                                  f: impl FnOnce(&mut C)
                                                    + 'static
                                                    + Send
                                                    + Sync)
                                                  -> Self {
    (move |world: &mut World| mutate_component(world, entity, f)).into()
  }

  pub fn insert_player_component<C: Component + 'static>(component: C) -> Self {
    (move |world: &mut World| {
      if let Some(player_entity) = get_player(world) {
        insert_component(world, player_entity, component);
      }
    }).into()
  }

  pub fn update_player_component<C: Component + Clone + 'static>(f: impl FnOnce(C) -> C
                                                                   + 'static
                                                                   + Send
                                                                   + Sync)
                                                                 -> Self {
    (move |world: &mut World| {
      if let Some(player_entity) = get_player(world) {
        update_component(world, player_entity, f);
      }
    }).into()
  }

  pub fn mutate_player_component<C: Component + Clone + 'static>(f: impl FnOnce(&mut C)
                                                                   + 'static
                                                                   + Send
                                                                   + Sync)
                                                                 -> Self {
    (move |world: &mut World| {
      if let Some(player_entity) = get_player(world) {
        mutate_component(world, player_entity, f);
      }
    }).into()
  }
}

impl Command for MyCommand {
  fn apply(self, world: &mut World) { (self.0)(world); }
}

// fn combat_actions()

#[derive(Component, Clone)]
pub enum VisualEffect {
  Laser { target: Entity, shooter: Entity },
  Missile { target: Entity, init_pos: Vec3 },
  Explosion { pos: Vec3 }
}
const LASER_DURATION_TICKS: u32 = 78;
const LASER_DAMAGE: u32 = 10;

impl VisualEffect {
  fn specify_transform(&self,
                       query: &Query<&Transform, Without<VisualEffect>>,
                       age: u32)
                       -> Option<Transform> {
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
        {
          let start_pos = shooter_transform.translation;
          let target_pos = target_transform.translation;
          let distance = start_pos.distance(target_pos);
          let center_pos = (start_pos + target_pos) * 0.5;
          let max_laser_radius = 0.18;
          let laser_radius =
            max_laser_radius
            * f32::sin(PI * time_left as f32 / LASER_DURATION_TICKS as f32).powf(0.4);

          Some(Transform::from_translation(center_pos).looking_at(target_pos, Vec3::Y)
               .with_scale(vec3(laser_radius,
                                laser_radius,
                                distance * 0.5)))
        } else {
          None
        }
      }
      VisualEffect::Missile { target, init_pos } => {
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
  (VisualEffect::Explosion { pos },
   Visuals::material_sphere(MyMaterial::ExplosionMaterial),
   SpatialBundle::default())
}
pub fn laser_visual(shooter: Entity, target: Entity) -> impl Bundle {
  (VisualEffect::Laser { target, shooter },
   Visuals::material_mesh(MyMaterial::LaserMaterial, GenMesh::Sphere),
   SpatialBundle::default())
}
fn missile_visual(init_pos: Vec3, target: Entity) -> impl Bundle {
  (VisualEffect::Missile { init_pos, target },
   Visuals::material_sphere(MyMaterial::GlowyMaterial3),
   SpatialBundle::default())
}

#[derive(Component)]
struct OriginTime(u32);
fn origin_time(q: Query<Entity, Without<OriginTime>>,
               time_ticks: Res<TimeTicks>,
               mut c: Commands) {
  for e in &q {
    c.entity(e).insert(OriginTime(time_ticks.0));
  }
}
fn combat_visual_effects(transformq: Query<&Transform, Without<VisualEffect>>,
                         mut visualq: Query<(Entity,
                                &mut Transform,
                                &VisualEffect,
                                &OriginTime)>,
                         time_ticks: Res<TimeTicks>,
                         mut c: Commands) {
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
  Missile,
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
  FireLaser(u32),
  FireMissile(u32),
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
pub fn combat_system(mut c: Commands,
                     time: Res<TimeTicks>,
                     mut combat_query: Query<(Entity,
                            &Transform,
                            &mut Combat,
                            Option<&IsHostile>,
                            &InZone)>,
                     player_query: Query<(Entity, &Transform, &Player)>) {
  let modval = time.0 % COMBAT_INTERVAL_TICKS;
  if modval != 0 {
    if modval >= (COMBAT_INTERVAL_TICKS / 3) {
      for (entity, transform, mut combat, _, _) in &mut combat_query {
        if combat.hp_depleted() {
          c.spawn(explosion_visual(transform.translation, 2.0));
          c.entity(entity).despawn_recursive();
        }
      }
    }
    return;
  }

  if let Ok((player_entity, player_transform, player)) = player_query.get_single() {
    let player_pos = player_transform.translation;
    let player_action =
      player.target().map_or(CombatAction::None, |target| {
                       CombatAction::Targeted(TargetedAbility::FireLaser(10), target)
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
          CombatAction::Targeted(TargetedAbility::FireLaser(5), player_entity)
        } else {
          CombatAction::Targeted(TargetedAbility::FireMissile(5), player_entity)
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
                TargetedAbility::FireLaser(n) => {
                  // println("cccc");
                  target_combat.decrease_hp(n);
                  c.spawn(laser_visual(self_entity, target));
                }
                TargetedAbility::FireMissile(n) => {
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

fn filter_least_map<O: Ord + Clone, T, R>(f: impl Fn(T) -> Option<(R, O)>,
                                          coll: impl IntoIterator<Item = T>)
                                          -> Option<R> {
  coll.into_iter()
      .filter_map(f)
      .min_by_key(|(_, o)| o.clone())
      .map(|(r, _)| r)
}

fn filter_least<O: Ord + Clone, T>(f: impl Fn(&T) -> Option<O>,
                                   coll: impl IntoIterator<Item = T>)
                                   -> Option<T> {
  filter_least_map(|t| f(&t).map(|v| (t, v)), coll)
}
fn filter_most_map<O: Ord + Clone, T, R>(f: impl Fn(T) -> Option<(R, O)>,
                                         coll: impl IntoIterator<Item = T>)
                                         -> Option<R> {
  coll.into_iter()
      .filter_map(f)
      .max_by_key(|(_, o)| o.clone())
      .map(|(r, _)| r)
}
fn filter_most<O: Ord + Clone, T>(f: impl Fn(&T) -> Option<O>,
                                  coll: impl IntoIterator<Item = T>)
                                  -> Option<T> {
  filter_most_map(|t| f(&t).map(|v| (t, v)), coll)
}
const ENEMY_SEE_PLAYER_RANGE: f32 = 100.0;
fn player_target_interaction(keys: Res<ButtonInput<KeyCode>>,
                             mut playerq: Query<(Entity,
                                    &mut Player,
                                    &Transform,
                                    &mut Combat)>,
                             mut hostileq: Query<(Entity, &IsHostile, &Transform)>,
                             mut c: Commands,
                             time: Res<TimeTicks>,
                             targetq: Query<(&Transform,)>) {
  let shoot_time_between = 60;
  let can_see_target = |e| true;
  if let Ok((player_entity, mut player, player_transform, mut player_combat)) =
    playerq.get_single_mut()
  {
    let player_pos = player_transform.translation;

    if keys.just_pressed(KeyCode::KeyQ) {
      player_combat.shield = !player_combat.shield;
    }
    if keys.just_pressed(KeyCode::KeyT) {
      if let Some((e, _, _)) =
        filter_least(|(e, hostile, transform)| {
                       hostile.0
                              .then_some(transform.translation.distance(player_pos) as u32)
                     },
                     &hostileq)
      {
        player.set_target(e);
      }
    }
    if let Some(state) = player.target_interaction_state.as_mut()
       && let Ok((target_transform)) = targetq.get(state.target)
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
}

#[derive(Component, Default, Clone)]
pub struct Container(pub HashSet<Entity>);
impl Container {
  pub fn empty() -> Container { Container::default() }
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
fn avg<T: std::iter::Sum + std::ops::Div<f32, Output = T>>(coll: impl IntoIterator<Item = T>)
                                                           -> Option<T> {
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
pub fn click_target(mut parent_q: Query<&Parent>,
                    mut click_events: EventReader<bevy_mod_picking::events::Pointer<bevy_mod_picking::events::Click>>,
                    mut player_q: Query<&mut Player>) {
  if let Ok(mut player) = player_q.get_single_mut() {
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
}
// type ClickTargetChild = (PbrBundle,
//                          NotShadowCaster,
//                          NotShadowReceiver,
//                          Highlight<StandardMaterial>,
//                          PickableBundle);
#[derive(Bundle, Clone)]
pub struct SpaceObjectBundle((SpaceObject,
                               Visuals,
                               LockedAxes,
                               ColliderMassProperties,
                               Collider,
                               RigidBody,
                               LinearDamping,
                               AngularDamping,
                               LinearVelocity,
                               AngularVelocity,
                               ExternalForce,
                               ExternalImpulse,
                               SpatialBundle));
impl SpaceObjectBundle {
  fn new(translation: Vec3, scale: f32, can_move: bool, visuals: Visuals) -> Self {
    let collider = Collider::sphere(1.0);
    Self((SpaceObject { scale, ..default() },
          visuals,
          LockedAxes::ROTATION_LOCKED,
          ColliderMassProperties::new(&collider, 1.0),
          collider,
          if can_move {
            RigidBody::Dynamic
          } else {
            RigidBody::Static
          },
          LinearDamping(1.6),
          AngularDamping(1.2),
          LinearVelocity::default(),
          AngularVelocity::default(),
          ExternalForce::default().with_persistence(false),
          ExternalImpulse::default(),
          SpatialBundle { transform: Transform { translation,
                                                 rotation: default(),
                                                 scale: Vec3::splat(scale) },
                          ..default() }))
  }
  fn sprite(translation: Vec3, scale: f32, can_move: bool, sprite: MySprite) -> Self {
    Self::new(translation, scale, can_move, Visuals::sprite(sprite))
  }
}

fn camera_follow_player(mut camq: Query<&mut PanOrbitCamera>,
                        playerq: Query<&Transform, With<Player>>) {
  if let Ok(player_transform) = playerq.get_single()
     && let Ok(mut cam) = camq.get_single_mut()
  {
    cam.target_focus = player_transform.translation;
  }
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
struct Navigation {
  max_thrust: f32,
  #[new(default)]
  navigation_kind: NavigationKind
}

fn navigation(mut navigators_q: Query<(&Navigation,
                     &Transform,
                     &mut ExternalForce,
                     &mut LinearVelocity)>,
              chase_targets_q: Query<&GlobalTransform>) {
  for (&Navigation { max_thrust,
                     navigation_kind },
       &Transform { translation, .. },
       mut force,
       velocity) in &mut navigators_q
  {
    match navigation_kind {
      NavigationKind::None => {}
      NavigationKind::Dir3(dir) => {
        force.apply_force(dir.as_vec3() * max_thrust);
      }
      NavigationKind::Vec3(vec) => {
        force.apply_force(vec.normalize_or_zero() * max_thrust);
      }
      NavigationKind::Pos(pos) => {
        force.apply_force((pos - translation).normalize_or_zero() * max_thrust);
      }
      NavigationKind::Chase(entity) => {
        if let Ok(entity_globaltransform) = chase_targets_q.get(entity)
           && let entity_pos = entity_globaltransform.translation()
           && let rel = (entity_pos - translation)
        {
          force.apply_force(rel.normalize_or_zero() * max_thrust);
        };
      }
      NavigationKind::ChaseAtRange(entity, range) => {
        if let Ok(entity_globaltransform) = chase_targets_q.get(entity)
           && let entity_pos = entity_globaltransform.translation()
           && let rel = (entity_pos - translation)
           && let within_range = rel.length() < range
        {
          force.apply_force(rel.normalize_or_zero()
                            * max_thrust
                            * if within_range { -1.0 } else { 1.0 });
        };
      }
    }
  }
}
const PLAYER_FORCE: f32 = 170.0;
const PLAYER_SCALE: f32 = 1.2;
fn player(translation: Vec3) -> impl Bundle {
  (Player::default(),
   name("You"),
   Combat { hp: 400,
            ..default() },
   Inventory::default(),
   Navigation::new(PLAYER_FORCE),
   CanBeFollowedByNPC,
   SpaceObjectBundle::new(translation,
                          PLAYER_SCALE,
                          true,
                          Visuals::sprite(MySprite::SpaceshipWhite)))
}
pub fn player_movement(keyboard_input: Res<ButtonInput<KeyCode>>,
                       camq: Query<&Transform, With<Camera3d>>,
                       mut globaltransform_q: Query<&GlobalTransform>,
                       mut playerq: Query<(Entity,
                              &SpaceObject,
                              &mut Navigation,
                              &mut ExternalForce,
                              &mut ExternalImpulse,
                              &mut LinearVelocity,
                              &Transform,
                              &mut Player)>) {
  if let (Ok((player_entity,
              player_space_object,
              mut player_navigation,
              mut player_force,
              mut player_impulse,
              mut player_vel,
              player_transform,
              mut player)),
          Ok(cam_transform)) = (playerq.get_single_mut(), camq.get_single())
  {
    let up = Vec3::Y;
    let forward = Vec3 { y: 0.0,
                         ..cam_transform.forward().into() }.normalize_or_zero();
    let right = forward.cross(up);

    let Vec3 { x, y, z } =
      sum(filter_map(|(key, v)| keyboard_input.pressed(key).then_some(v),
                     [(KeyCode::KeyA, Vec3::NEG_X),
                      (KeyCode::KeyS, Vec3::NEG_Z),
                      (KeyCode::KeyD, Vec3::X),
                      (KeyCode::KeyW, Vec3::Z),
                      (KeyCode::ControlLeft, Vec3::NEG_Y),
                      (KeyCode::ShiftLeft, Vec3::Y)])).normalize_or_zero();
    let keyb_dir = (x * right) + (z * forward) + (y * up);
    player_navigation.navigation_kind = if let Some(PlayerTargetInteractionState { target,
                                                                                   approaching,
                                                                                   .. }) =
      player.target_interaction_state && approaching
    {
      NavigationKind::Chase(target)
    } else {
      NavigationKind::Vec3(keyb_dir)
    };
  }
}
#[derive(Bundle)]
struct SceneSpaceObjectBundle((Handle<Scene>, SpaceObjectBundle));
impl SceneSpaceObjectBundle {
  fn new(translation: Vec3, scale: f32, can_move: bool, scene: Handle<Scene>) -> Self {
    Self((scene,
          SpaceObjectBundle::new(translation,
                                 scale,
                                 can_move,
                                 Visuals::sprite(MySprite::Coffee))))
  }
}
// pub const RAPIER_CONFIG: RapierConfiguration =
//   RapierConfiguration { gravity: Vec3::ZERO,
//                         physics_pipeline_active: true,li
//                         query_pipeline_active: true,
//                         timestep_mode: todo!(),
//                         scaled_shape_subdivision: todo!(),
//                         force_update_from_transform_changes: todo!() };

pub fn spawn_mushroom_man(playerq: Query<&Transform, With<Player>>,
                          keyboard_input: Res<ButtonInput<KeyCode>>,
                          mut c: Commands) {
  if let Ok(&player_transform) = playerq.get_single()
     && keyboard_input.just_pressed(KeyCode::KeyZ)
  {
    debug_println(player_transform);
    c.spawn(mushroom_man(player_transform.translation));
  }
}

pub fn warp(mut playerq: Query<&mut Transform, With<Player>>,
            targetq: Query<&GlobalTransform>,
            keyboard_input: Res<ButtonInput<KeyCode>>) {
  if let Ok(mut player_transform) = playerq.get_single_mut()
     && let Some(target_globaltransform) = pick(&targetq)
     && keyboard_input.just_pressed(KeyCode::KeyG)
  {
    player_transform.translation = target_globaltransform.translation();
  }
}
#[derive(Default, Resource)]
pub struct TimeTicks(pub u32);
pub fn increment_time(mut time: ResMut<TimeTicks>) { time.0 += 1; }
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

fn close_on_esc(mut exit: EventWriter<AppExit>, keyboard_input: Res<ButtonInput<KeyCode>>) {
  if keyboard_input.just_pressed(KeyCode::Escape) {
    exit.send(AppExit::Success);
  }
}

#[derive(Resource, Default)]
pub enum GameState {
  #[default]
  FlyingInSpace,
  WarpGui(ui::WarpGui)
}
#[derive(Event, Clone, Copy)]
pub enum GuiInputEvent {
  Up,
  Down,
  Left,
  Right,
  Select,
  Cancel
}
enum Alignment {
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
#[derive(Eq, PartialEq, Clone, Copy, Assoc, Default, Debug)]
#[func(pub const fn alignment(&self) -> Alignment)]
pub enum Faction {
  #[default]
  #[assoc(alignment = Alignment::Neutral)]
  Wanderers,
  #[assoc(alignment = Alignment::LawfulGood)]
  SpacePolice,
  #[assoc(alignment = Alignment::ChaoticEvil)]
  SpacePirates,
  #[assoc(alignment = Alignment::ChaoticNeutral)]
  SpaceWizards,
  #[assoc(alignment = Alignment::NeutralGood)]
  Traders,
  #[assoc(alignment = Alignment::LawfulEvil)]
  Invaders
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
    matches!(self.alignment(),
             Alignment::LawfulGood | Alignment::NeutralGood | Alignment::ChaoticGood)
  }
  fn is_bad(&self) -> bool { !self.is_good() }
  fn is_hostile(&self, target: Self) -> bool {
    (self.is_bad() || target.is_bad()) && (*self != target)
  }
}
fn colorful_texture() -> Image {
  let texture_size = 8;
  Image::new_fill(bevy::render::render_resource::Extent3d { width: texture_size,
                                                            height: texture_size,
                                                            depth_or_array_layers: 1 },
                  bevy::render::render_resource::TextureDimension::D2,
                  map(|_| rand::random(),
                      0..((texture_size * texture_size * 4) as usize)).collect::<Vec<u8>>()
                                                                      .as_slice(),
                  bevy::render::render_resource::TextureFormat::Rgba8UnormSrgb,
                  bevy::render::render_asset::RenderAssetUsages::RENDER_WORLD)
}

#[derive(Component, Clone)]
pub struct Enemy;

pub fn scaled_npc(pos: Vec3,
                  scale: f32,
                  name: &'static str,
                  thrust: f32,
                  faction: Faction,
                  hp: u32,
                  sprite: MySprite)
                  -> impl Bundle {
  (Name::new(name),
   Navigation::new(thrust),
   NPC { follow_target: None,
         faction },
   Combat { hp, ..default() },
   SpaceObjectBundle::new(pos, scale, true, Visuals::sprite(sprite)))
}
pub struct NamedNPC {
  name: &'static str,
  faction: Faction,
  sprite: MySprite,
  dialogue_tree: DialogueTree
}
type DialogueEffect = fn() -> MyCommand;
type DialogueTreeNode = (&'static str, &'static [(&'static str, &'static str, &'static str, Option<DialogueEffect>)]);
const DIALOGUE_END:DialogueTreeNode = ("END", &[]);
type DialogueTree = &'static [DialogueTreeNode];

pub const SPHERICAL_SPACE_COW:DialogueTree = &[
  ("A", &[
    ("B", "Hello there, cow!", "Cow: \"Moo-stronaut reporting for duty!\"", None),
  ]),
  ("B", &[
    ("C", "Why are you in space?", "Cow: \"I'm here to study zero-gravity milk production!\"", None),
    ("D", "How did you become spherical?", "Cow: \"It's an evolutionary adaptation for space travel.\"", None),
    ("E", "Are you lost?", "Cow: \"No, space is my new pasture!\"", None),
  ]),
  ("C", &[
    ("F", "How's the milk production going?", "Cow: \"It's out of this world! Want to try some Moon Moo?\"", None),
    ("G", "Isn't it dangerous up here?", "Cow: \"No need to cow-er in fear, I've got my space suit!\"", None),
    ("H", "Who sent you here?", "Cow: \"Dr. Bovine von Lactose, the mad dairy scientist!\"", None),
  ]),
  ("D", &[
    ("I", "What are the advantages of being spherical?", "Cow: \"I can roll in any direction, and there are no corners to bump into!\"", None),
    ("J", "Are there other spherical animals in space?", "Cow: \"I've heard rumors of a cubical chicken, but that's just absurd.\"", None),
    ("K", "Can you change back to normal?", "Cow: \"Why would I? Being spherical is utterly amazing!\"", None),
  ]),
  ("E", &[
    ("L", "Don't you miss Earth?", "Cow: \"Sometimes, but the view up here is spe-cow-tacular!\"", None),
    ("M", "What do you eat in space?", "Cow: \"Cosmic ray grass and star dust. It's quite moo-tritious!\"", None),
    ("N", "How do you moo-ve around?", "Cow: \"I just roll with it! Newton's laws are my best friends.\"", None),
  ]),
  ("F", &[
    ("O", "Yes, I'd love to try some!", "Cow: \"Here's a glass of Moon Moo. It's extra frothy in zero-G!\"", None),
  ]),
  ("G", &[
    ("P", "What's the biggest danger you've faced?", "Cow: \"I once got caught in Saturn's rings. Talk about a tight spot!\"", None),
  ]),
  ("H", &[
    ("Q", "Can I meet this scientist?", "Cow: \"He's on the dark side of the moon. It's a bit of a trek!\"", None),
  ]),
  ("I", &[
    ("R", "Can you demonstrate your rolling?", "Cow: \"Sure! Watch me do a barrel roll!\"", None),
  ]),
  ("J", &[
    ("S", "A cubical chicken? That's crazy!", "Cow: \"I know, right? Geometry in space gets wild!\"", None),
  ]),
  ("K", &[
    ("T", "Do you ever get dizzy from being round?", "Cow: \"Nope, I'm always well-balanced!\"", None),
  ]),
  ("L", &[
    ("U", "What's your favorite view from space?", "Cow: \"The Milky Way, of course! It reminds me of home.\"", None),
  ]),
  ("M", &[
    ("V", "Does star dust taste good?", "Cow: \"It's a bit dry, but it makes my milk sparkle!\"", None),
  ]),
  ("N", &[
    ("W", "Can you explain the physics of your movement?", "Cow: \"It's all about conservation of moo-mentum!\"", None),
  ]),
  ("O", &[
    ("X", "Wow, it's delicious! Can I have the recipe?", "Cow: \"Sorry, it's a closely guarded secret of the cosmos.\"", None),
  ]),
  ("P", &[
    ("Y", "That sounds terrifying! How did you escape?", "Cow: \"I used my quick re-flex-es and dairy-ing escape plan!\"", None),
  ]),
  ("Q", &[
    ("Z", "Is he planning to send more animals to space?", "Cow: \"He's working on a flock of zero-gravity sheep as we speak!\"", None),
  ]),
  ("R", &[
    ("AA", "Impressive! Do you ever get motion sick?", "Cow: \"Nah, I've got a stomach of steel... er, four of them actually!\"", None),
  ]),
  ("S", &[
    ("AB", "Are there any other strange space animals?", "Cow: \"I've heard whispers of a dodecahedron dolphin, but that's just silly.\"", None),
  ]),
  ("T", &[
    ("AC", "You're full of jokes! Are all space cows this funny?", "Cow: \"Of course! Humor helps us cope with the uni-verse-al challenges.\"", None),
  ]),
  ("U", &[
    ("AD", "That's beautiful. Do you ever feel lonely up here?", "Cow: \"Sometimes, but then I remember I'm surrounded by stars... and star-struck fans like you!\"", None),
  ]),
  ("V", &[
    ("AE", "Sparkly milk sounds amazing! Can it grant superpowers?", "Cow: \"Only the power of good bone density and a happy tummy!\"", None),
  ]),
  ("W", &[
    ("AF", "You're quite the physicist! Ever thought of teaching?", "Cow: \"I've been thinking of starting a 'Moo-niversity' actually!\"", None),
  ]),
  ("X", &[
    ("END", "I understand. Thanks for sharing it with me!", "Cow: \"You're welcome! Remember, what happens in space, stays in space!\"", None),
  ]),
  ("Y", &[
    ("END", "You're quite the adventurer! Any other close calls?", "Cow: \"Well, there was this one time with a black hole... but that's a story for another day!\"", None),
  ]),
  ("Z", &[
    ("END", "Wow! What's next, pigs in orbit?", "Cow: \"Don't be silly! Everyone knows pigs can't fly... yet.\"", None),
  ]),
  ("AA", &[
    ("END", "You're amazing! Can I take a selfie with you?", "Cow: \"Of course! Let's make it a 'span-selfie' - spanning the cosmos!\"", None),
  ]),
  ("AB", &[
    ("END", "This is getting too weird. I think I need to go.", "Cow: \"Aw, don't have a cow, man! Stay a while and listen to more space tales!\"", None),
  ]),
  ("AC", &[
    ("END", "You're out of this world! Thanks for the chat!", "Cow: \"My pleasure! Remember, in space, everyone can hear you cream... your coffee!\"", None),
  ]),
  ("AD", &[
    ("END", "You're never alone with that attitude! Goodbye, space cow!", "Cow: \"Goodbye, Earth friend! May your dreams be as boundless as the universe!\"", None),
  ]),
  ("AE", &[
    ("END", "I'll take a gallon! This was fun, thanks!", "Cow: \"Come back anytime! The Milky Way's always open!\"", None),
  ]),
  ("AF", &[
    ("END", "Sign me up for Astro-nomoo-my 101! Farewell!", "Cow: \"So long, and thanks for all the laughs! Keep reaching for the stars!\"", None),
  ]),
  ("END", &[]),
];

pub const SPACE_COWBOY: DialogueTree = &[
    ("A", &[
        ("B", "Howdy, partner!", "Space Cowboy: \"Well, howdy there, space traveler! Welcome to the cosmic corral!\"", None),
    ]),
    ("B", &[
        ("C", "What's a cowboy doing in space?", "Space Cowboy: \"Roundin' up asteroids and headin' off comet stampedes, of course!\"", None),
        ("D", "Nice space suit! Is that leather?", "Space Cowboy: \"Sure is! Genuine Martian leather, tougher than a solar flare!\"", None),
        ("E", "Have you seen any aliens?", "Space Cowboy: \"Aliens? Why, I've shared a campfire with beings from more galaxies than you can count!\"", None),
    ]),
    ("C", &[
        ("F", "Asteroid roundup? How does that work?", "Space Cowboy: \"With a quantum lasso and a whole lotta patience, partner!\"", None),
        ("G", "Comet stampedes sound dangerous!", "Space Cowboy: \"You bet your stars they are! But nothin' my trusty rocket horse can't handle.\"", None),
    ]),
    ("D", &[
        ("H", "Martian leather? Is that ethical?", "Space Cowboy: \"Now, don't you worry. It's all synthetic, made from Mars dust. No space cows harmed!\"", None),
        ("I", "How does it protect you from space?", "Space Cowboy: \"It's lined with nanotech fibers. Keeps out cosmic rays better than a fort keeps out rustlers!\"", None),
    ]),
    ("E", &[
        ("J", "Tell me about these aliens!", "Space Cowboy: \"Met a cloud being from Nebula Nine once. Makes a mean vapor coffee!\"", None),
        ("K", "A cosmic campfire? How?", "Space Cowboy: \"With a contained plasma flame, 'course! Roasts space marshmallows like you wouldn't believe.\"", None),
    ]),
    ("F", &[
        ("END", "That sounds amazing! Can you teach me?", "Space Cowboy: \"Sure thing, greenhorn! First lesson: always approach an asteroid from downwind.\"", None),
    ]),
    ("G", &[
        ("END", "A rocket horse? Now I've heard everything!", "Space Cowboy: \"Ol' Supernova here's been my loyal steed for light-years! Ain't ya, girl?\" *pats invisible horse*", None),
    ]),
    ("H", &[
        ("END", "That's a relief! It looks so realistic.", "Space Cowboy: \"Yep, fools even the keenest eye. Now, if you'll excuse me, I've got some solar wind to wrangle!\"", None),
    ]),
    ("I", &[
        ("END", "Incredible! Where can I get one?", "Space Cowboy: \"These suits are rarer than a quiet night in a neutron star saloon. But if you prove yourself, I might know a fella...\"", None),
    ]),
    ("J", &[
        ("END", "Vapor coffee? That's wild!", "Space Cowboy: \"Puts hair on your chest and a twinkle in your eye! Now, if you'll pardon me, I've got a date with the Milky Way.\"", None),
    ]),
    ("K", &[
        ("END", "Space marshmallows? Now I'm hungry!", "Space Cowboy: \"Tell ya what, next time you're in the Andromeda arm, look me up. We'll have ourselves a good ol' space hoedown!\"", None),
    ]),
    ("END", &[]),
];

pub const SOCRATES_DIALOGUE: DialogueTree = &[
    ("A", &[
        ("B", "Greetings, Socrates! How are you finding space?", "Socrates: \"Ah, greetings, young seeker of knowledge! Space, like wisdom, is vast and full of wonder.\"", None),
    ]),
    ("B", &[
        ("C", "What do you think about this future world?", "Socrates: \"I know that I know nothing of this world, which makes it all the more fascinating to question and explore.\"", None),
        ("D", "Can you tell me about your philosophical method?", "Socrates: \"Even here, amidst the stars, we must question everything. Shall we examine the nature of this cosmic realm?\"", None),
        ("E", "How does space travel relate to your ideas of the soul?", "Socrates: \"Perhaps our souls, like these celestial bodies, are on an eternal journey through the universe of ideas.\"", None),
    ]),
    ("C", &[
        ("F", "Does this advanced technology change your views on knowledge?", "Socrates: \"Technology may advance, but the pursuit of wisdom remains unchanged. We must still question and reflect.\"", None),
        ("G", "What would you ask the aliens if we meet them?", "Socrates: \"I would ask them about their concept of virtue, and whether it's universal across the cosmos.\"", None),
    ]),
    ("D", &[
        ("H", "How would you apply the Socratic method to space exploration?", "Socrates: \"We must question our assumptions about the universe, just as we question our beliefs about ourselves.\"", None),
        ("I", "Can your ideas of ethics apply to alien civilizations?", "Socrates: \"The search for universal truths should extend beyond Earth. Perhaps aliens too seek the good life.\"", None),
    ]),
    ("E", &[
        ("J", "Do you think space travel could be a form of seeking truth?", "Socrates: \"Indeed! As we journey through space, are we not also journeying through the realm of ideas?\"", None),
        ("K", "How does floating in space compare to your concept of the Forms?", "Socrates: \"This weightlessness reminds me of how the soul must feel when contemplating the Forms. Utterly free!\"", None),
    ]),
    ("F", &[
        ("END", "That's profound. Thank you for your wisdom, Socrates.", "Socrates: \"Remember, the unexamined space life is not worth living! Now, shall we ponder the ethics of faster-than-light travel?\"", None),
    ]),
    ("G", &[
        ("END", "Alien virtue? That's a mind-bending concept!", "Socrates: \"Indeed! And in questioning them, we may learn more about ourselves. Now, I wonder if there's a cosmic equivalent of hemlock...\"", None),
    ]),
    ("H", &[
        ("END", "I see. Question everything, even in space!", "Socrates: \"Precisely! Now, let us question the very nature of these asteroid fields. What is their essence?\"", None),
    ]),
    ("I", &[
        ("END", "Universal ethics across species... fascinating!", "Socrates: \"A worthy pursuit indeed! Now, if you'll excuse me, I must go contemplate the allegory of the black hole.\"", None),
    ]),
    ("J", &[
        ("END", "Space travel as a metaphor for seeking truth. Brilliant!", "Socrates: \"You show wisdom, young space traveler. Now, shall we examine the true form of these twinkling stars?\"", None),
    ]),
    ("K", &[
        ("END", "Your ideas truly transcend time and space, Socrates.", "Socrates: \"As do all ideas, my friend. Now, I must float away and dialectically analyze this cosmic dust.\"", None),
    ]),
];

pub const MARIE_CURIE_DIALOGUE: DialogueTree = &[
    ("A", &[
        ("B", "Madame Curie! It's an honor. How are you adapting to space?", "Marie Curie: \"Bonjour! The universe is full of natural marvels. I'm detecting fascinating radiation patterns!\"", None),
    ]),
    ("B", &[
        ("C", "What do you think about modern space technology?", "Marie Curie: \"C'est incroyable! The advances in physics and chemistry have led to marvels beyond my wildest dreams.\"", None),
        ("D", "How does your work on radioactivity apply here?", "Marie Curie: \"The principles remain the same, but the scale is enormous! Cosmic rays, solar radiation... so much to study!\"", None),
        ("E", "What would you like to research in space?", "Marie Curie: \"I'm fascinated by the potential for new elements in these asteroids. Shall we start collecting samples?\"", None),
    ]),
    ("C", &[
        ("F", "Do you think space travel would have changed your research?", "Marie Curie: \"Undoubtedly! The absence of gravity opens up new possibilities for experiments in radioactivity.\"", None),
        ("G", "What advice would you give to future scientists?", "Marie Curie: \"Never fear the unknown. In science and in space, curiosity is our greatest asset.\"", None),
    ]),
    ("D", &[
        ("H", "How would you protect astronauts from cosmic radiation?", "Marie Curie: \"We must study it first! Understanding radiation is key to protection. Perhaps a new element could help...\"", None),
        ("I", "Could your work on X-rays be applied to space medicine?", "Marie Curie: \"Absolutely! Imagine a portable X-ray device for diagnosing injuries on long space voyages.\"", None),
    ]),
    ("E", &[
        ("J", "What kind of lab equipment would you need for space research?", "Marie Curie: \"A spectrometer would be essential. And perhaps we could design a microgravity centrifuge for separation!\"", None),
        ("K", "Do you think we might find radioactive alien life?", "Marie Curie: \"An intriguing hypothesis! We must approach it with rigorous scientific method and an open mind.\"", None),
    ]),
    ("F", &[
        ("END", "Your passion for science is truly inspiring, Madame Curie.", "Marie Curie: \"Merci! Remember, in science as in space exploration, we must have perseverance and faith in the unknown.\"", None),
    ]),
    ("G", &[
        ("END", "Great advice! Science and exploration go hand in hand.", "Marie Curie: \"Indeed! Now, shall we analyze the spectral lines of that nearby star? For science!\"", None),
    ]),
    ("H", &[
        ("END", "A new element for radiation shielding? Brilliant idea!", "Marie Curie: \"Every discovery opens new doors. Now, let's calibrate this space-suited Geiger counter!\"", None),
    ]),
    ("I", &[
        ("END", "Space X-rays... that could revolutionize long-distance space travel!", "Marie Curie: \"Exactement! Science knows no borders, not even in the vastness of space. Now, where did I put my radium samples...\"", None),
    ]),
    ("J", &[
        ("END", "A space lab sounds amazing. You're already adapting to the future!", "Marie Curie: \"Science evolves, but the spirit of inquiry remains. Now, let's see what secrets these cosmic rays hold!\"", None),
    ]),
    ("K", &[
        ("END", "Radioactive aliens? Now that's a sci-fi concept!", "Marie Curie: \"Science often surpasses fiction! Now, help me set up this zero-gravity polonium experiment, s'il vous plaît.\"", None),
    ]),
];

pub const ABRAHAM_LINCOLN_DIALOGUE: DialogueTree = &[
    ("A", &[
        ("B", "President Lincoln! How are you finding the space age?", "Lincoln: \"Four score and seven light-years ago... I jest. This future is both terrifying and awe-inspiring.\"", None),
    ]),
    ("B", &[
        ("C", "How do your ideas of democracy apply to space colonization?", "Lincoln: \"A government of the planets, by the planets, for the planets, shall not perish from this universe.\"", None),
        ("D", "What do you think about the current state of equality?", "Lincoln: \"Progress has been made, but our journey continues. We must ensure liberty and justice for all sentient beings.\"", None),
        ("E", "How would you handle diplomacy with alien races?", "Lincoln: \"With malice toward none, with charity for all... even those with tentacles or exoskeletons.\"", None),
    ]),
    ("C", &[
        ("F", "Should every planet have equal representation?", "Lincoln: \"A house divided against itself cannot stand, even if that house spans galaxies. We must find a way to unite.\"", None),
        ("G", "What about AI rights in this futuristic society?", "Lincoln: \"The notion that all intelligences are created equal must extend to artificial ones too. It's the next frontier of rights.\"", None),
    ]),
    ("D", &[
        ("H", "Have we achieved your vision of equality?", "Lincoln: \"Progress is evident, but the work is never finished. We must strive to extend equality across the cosmos.\"", None),
        ("I", "How can we apply your principles to alien civilizations?", "Lincoln: \"The better angels of our nature must guide us in treating all sentient life with respect and dignity.\"", None),
    ]),
    ("E", &[
        ("J", "Would you still believe in preserving the Union on a galactic scale?", "Lincoln: \"The principles remain sound. We must work to form a more perfect Union, even among the stars.\"", None),
        ("K", "How would you address conflicts between human colonies and alien worlds?", "Lincoln: \"Let us strive on to finish the work we are in, to achieve and cherish a just and lasting peace among ourselves and all sentient beings.\"", None),
    ]),
    ("F", &[
        ("END", "A galactic democracy... that's a big idea, Mr. President!", "Lincoln: \"Indeed it is! Now, if you'll excuse me, I need to draft the Emancipation Proclamation for the robots of Neptune...\"", None),
    ]),
    ("G", &[
        ("END", "AI rights? You're adapting quickly to future issues!", "Lincoln: \"The principles of liberty are timeless, my friend. Now, shall we discuss the ethics of faster-than-light travel?\"", None),
    ]),
    ("H", &[
        ("END", "Your vision continues to inspire us, even in space.", "Lincoln: \"Remember, the struggle for equality is as vast as space itself. Now, I must contemplate the Gettysburg Address for Martians.\"", None),
    ]),
    ("I", &[
        ("END", "Respecting all sentient life... a noble goal for the future.", "Lincoln: \"Indeed. The task remaining before us is as great as the cosmos itself. Now, where can a man get a stovepipe helmet for his spacesuit?\"", None),
    ]),
    ("J", &[
        ("END", "A galactic Union... that's an incredible concept!", "Lincoln: \"The work of unity never ceases, my friend. Now, I believe I have a speech to give at the Andromeda Lincoln Memorial.\"", None),
    ]),
    ("K", &[
        ("END", "Your words of peace resonate even in the space age, sir.", "Lincoln: \"May they echo across the stars. Now, I must attend to pressing matters. I hear there's a vampire problem on the dark side of the moon...\"", None),
    ]),
];

pub const CHRONOS_SPACE_WIZARD_DIALOGUE: DialogueTree = &[
  ("A", &[
    ("B", "Who are you, and why have you brought these historical figures to space?", "Space Wizard: \"Greetings, cosmic traveler! I am Chronos, the Space Wizard of Time and Dimension. I have assembled these great minds for a grand purpose!\"", None),
  ]),
  ("B", &[
    ("C", "What is this grand purpose?", "Chronos: \"To solve the greatest challenges of the universe! These brilliant minds, combined with futuristic knowledge, might save reality itself!\"", None),
    ("D", "How did you bring them here?", "Chronos: \"With my Chrono-Spatial Translocator, of course! It plucks beings from their timestreams and deposits them here, fully adapted to space travel.\"", None),
    ("E", "Won't this disrupt the timeline?", "Chronos: \"Fear not! Once our task is complete, I shall return them to their exact moments in history, memories intact but disguised as vivid dreams.\"", None),
  ]),
  ("C", &[
    ("F", "What are these universal challenges?", "Chronos: \"The heat death of the universe, the reconciliation of quantum mechanics and general relativity, and the correct way to eat a cosmic sandwich in zero gravity!\"", None),
    ("G", "How can historical figures help with future problems?", "Chronos: \"Fresh perspectives, my friend! Sometimes the wisdom of the past is key to unlocking the mysteries of the future.\"", None),
  ]),
  ("D", &[
    ("H", "Is the Chrono-Spatial Translocator safe?", "Chronos: \"Mostly! There was that one incident with Cleopatra and the black hole, but we don't talk about that...\"", None),
    ("I", "Can anyone use this device?", "Chronos: \"Goodness, no! It requires a degree in Temporal Physics and a license from the Intergalactic Time Authority. Plus, really good spatial awareness.\"", None),
  ]),
  ("E", &[
    ("J", "What if they want to stay in the future?", "Chronos: \"An excellent question! But history must run its course. Their contributions in their own times are crucial to the development of humanity.\"", None),
    ("K", "Could their future knowledge change history?", "Chronos: \"Their memories of this adventure will fade upon return, leaving only subconscious inspiration. Clever, eh?\"", None),
  ]),
  ("F", &[
    ("END", "Those are... interesting challenges. Especially the sandwich one.", "Chronos: \"Never underestimate the importance of proper space cuisine! Now, excuse me while I explain quantum entanglement to Socrates.\"", None),
  ]),
  ("G", &[
    ("END", "I see. It's like a cosmic think tank!", "Chronos: \"Precisely! Now, if you'll pardon me, I need to stop Marie Curie from trying to split atoms on the ship.\"", None),
  ]),
  ("H", &[
    ("END", "Mostly safe? That's... reassuring.", "Chronos: \"Don't worry! The chances of accidental dinosaur materialization are very low this time. Now, where did I put that temporal stabilizer...\"", None),
  ]),
  ("I", &[
    ("END", "I see. So no borrowing it for weekend trips to the Renaissance.", "Chronos: \"I'm afraid not. Last time someone did that, we ended up with pizza in ancient Egypt. Now, I must calibrate the quantum flux capacitor!\"", None),
  ]),
  ("J", &[
    ("END", "That makes sense. It's a big responsibility.", "Chronos: \"Indeed it is! The burden of knowledge is heavy, but the fate of the cosmos is heavier. Now, I need to explain internet memes to Abe Lincoln.\"", None),
  ]),
  ("K", &[
    ("END", "Subconscious inspiration... very clever indeed!", "Chronos: \"Thank you! Now, if you'll excuse me, I need to prevent Nikola Tesla from rewiring our ship's power grid. Again.\"", None),
  ]),
];
pub const LEONARDO_DA_VINCI_NPC:DialogueTree  = &[
  ("A", &[
    ("B", "Leonardo da Vinci! How are you finding the future?", "Leonardo: \"Ah, the marvels of this age! My mind overflows with new inventions and artworks inspired by the cosmos!\"", None),
  ]),
  ("B", &[
    ("C", "What do you think of modern technology?", "Leonardo: \"Magnifico! Though I must say, I had already envisioned many of these contraptions. See this spacesuit? I'm improving its design as we speak!\"", None),
    ("D", "How does space travel compare to your flying machine concepts?", "Leonardo: \"It's beyond my wildest dreams! Yet, the principles of flight I studied apply even here. Observe how we maneuver through this asteroid field!\"", None),
    ("E", "Would you like to paint this cosmic scenery?", "Leonardo: \"Oh, if only I had my easel! The play of light on these celestial bodies... it's the ultimate study of chiaroscuro!\"", None),
  ]),
  ("C", &[
    ("F", "What improvements would you make to our technology?", "Leonardo: \"I've been sketching designs for more efficient solar sails and a da Vinci-style space station. Care to take a look?\"", None),
    ("G", "How does this era inspire your creativity?", "Leonardo: \"The blend of art and science here is exquisite! I'm particularly intrigued by your holographic displays. An art form in itself!\"", None),
  ]),
  ("D", &[
    ("H", "Could your studies on bird flight help with space maneuvering?", "Leonardo: \"Indubitably! The grace of a bird and the dance of a spacecraft are not so different. It's all about understanding flow and resistance.\"", None),
    ("I", "What do you think of modern aviation?", "Leonardo: \"It's a dream realized! Though I must say, these rockets seem a bit inelegant. Perhaps we could design something more... artistic?\"", None),
  ]),
  ("E", &[
    ("J", "How would you capture the essence of space in art?", "Leonardo: \"I would blend the mathematical precision of star charts with the fluid beauty of nebulae. A fusion of the scientific and the divine!\"", None),
    ("K", "Would you be interested in creating art with our future tools?", "Leonardo: \"Absolutely! Imagine the possibilities of sculpting with zero-gravity 3D printers or painting with light itself!\"", None),
  ]),
  ("F", &[
    ("END", "Your ideas could revolutionize space travel, even now!", "Leonardo: \"Grazie mille! Now, if you'll excuse me, I must discuss the golden ratio with that charming nebula over there.\"", None),
  ]),
  ("G", &[
    ("END", "Your excitement for blending art and science is contagious!", "Leonardo: \"Art, science, technology - they are all one in the pursuit of knowledge and beauty! Now, where did I leave my anti-gravity sketchbook?\"", None),
  ]),
  ("H", &[
    ("END", "Birds and spaceships... I never thought of it that way!", "Leonardo: \"Nature is the greatest teacher, even among the stars! Now, I must continue my studies on the aerodynamics of space debris.\"", None),
  ]),
  ("I", &[
    ("END", "An artistic rocket? That's an intriguing concept!", "Leonardo: \"Form and function in perfect harmony! Now, let me show you my preliminary sketches for a Vitruvian Spaceman...\"", None),
  ]),
  ("J", &[
    ("END", "Your cosmic art sounds breathtaking. I can't wait to see it!", "Leonardo: \"The universe itself is the ultimate masterpiece! Now, if you'll pardon me, I need to recalibrate the golden ratio for non-Euclidean space.\"", None),
  ]),
  ("K", &[
    ("END", "Sculpting in zero-g... Now that would be something to see!", "Leonardo: \"Indeed! The possibilities are as endless as space itself. Now, I must go - I have an appointment to exchange ideas with a sentient gas cloud!\"", None),
  ]),
  DIALOGUE_END,
];

pub const CLEOPATRA_NPC:DialogueTree = &[
  ("A", &[
    ("B", "Queen Cleopatra! How are you adapting to the space age?", "Cleopatra: \"Greetings, cosmic traveler. I must say, ruling a galactic empire would have been... intriguing.\"", None),
  ]),
  ("B", &[
    ("C", "How does space travel compare to sailing the Nile?", "Cleopatra: \"The Nile was but a stream compared to this river of stars. Though I do miss the crocodiles... perhaps we could find some space equivalents?\"", None),
    ("D", "What do you think about modern politics and diplomacy?", "Cleopatra: \"Politics, like the cosmos, is vast and complex. But whether on Earth or among the stars, alliances and strategy remain key.\"", None),
    ("E", "How would you apply your leadership skills in this era?", "Cleopatra: \"An empire among the stars... now that's an ambition worthy of a pharaoh! I would unite planets as I united Egypt and Rome.\"", None),
  ]),
  ("C", &[
    ("F", "Space crocodiles? That's an interesting idea!", "Cleopatra: \"Indeed! Every queen needs her guardians. Besides, I'm sure there are plenty of cosmic treasures to protect in this vast universe.\"", None),
    ("G", "What aspects of space exploration fascinate you most?", "Cleopatra: \"The diversity of worlds reminds me of the cultures along the Mediterranean. Each unique, yet connected by the cosmic seas.\"", None),
  ]),
  ("D", &[
    ("H", "How would you handle diplomacy with alien races?", "Cleopatra: \"With grace, wisdom, and a hint of mystery. Whether dealing with Romans or Reptilians, a grand entrance is essential.\"", None),
    ("I", "What lessons from your era apply to galactic politics?", "Cleopatra: \"Power is about perception and alliances. Even in space, one must know when to be the asp and when to be the charm.\"", None),
  ]),
  ("E", &[
    ("J", "A galactic empire? That's quite ambitious!", "Cleopatra: \"Go big or go home, as they say. Though in space, I suppose everywhere is home. First, we'll need a cosmic Alexandria...\"", None),
    ("K", "How would you structure a government across planets?", "Cleopatra: \"A pharaoh for each world, united under a galactic regent. Myself, naturally. With faster-than-light communication, governance should be a breeze.\"", None),
  ]),
  ("F", &[
    ("END", "I'll keep an eye out for space crocodiles, Your Majesty.", "Cleopatra: \"Do that, dear friend. Now, if you'll excuse me, I must review the blueprints for my orbital pyramid.\"", None),
  ]),
  ("G", &[
    ("END", "Your insight draws beautiful parallels, Your Highness.", "Cleopatra: \"Thank you. The universe, like Egypt, is full of hidden treasures. Now, I'm off to negotiate mining rights with the asteroid belt pharaohs.\"", None),
  ]),
  ("H", &[
    ("END", "Diplomacy through mystery and grandeur. Classic Cleopatra!", "Cleopatra: \"One must keep the mystique alive, even in a spacesuit. Now, be a dear and help me plan my zero-gravity barge procession.\"", None),
  ]),
  ("I", &[
    ("END", "The asp and the charm... a timeless strategy, it seems.", "Cleopatra: \"In politics, some things never change. Now, I must go charm the Arcturian ambassador. Or was it the Betelgeusian regent?\"", None),
  ]),
  ("J", &[
    ("END", "A cosmic Alexandria sounds magnificent!", "Cleopatra: \"Doesn't it? With a library containing the knowledge of a million worlds! Now, if you'll excuse me, I need to discuss funding with the Galactic Senate.\"", None),
  ]),
  ("K", &[
    ("END", "Your administrative skills are truly universal, Your Highness.", "Cleopatra: \"Naturally. Now, I must go. These star charts won't decipher themselves, and I have a galaxy to unite!\"", None),
  ]),
  DIALOGUE_END,
];
// pub fn talking_npc(pos: Vec3,
//                    scale: f32,
//                    name: &'static str,
//                    thrust: f32,
//                    faction: Faction,
//                    hp: u32,
//                    sprite: MySprite
// )
//                    -> impl Bundle {
//   (Name::new(name),
//    Navigation::new(thrust),
//    NPC { follow_target: None,
//          faction },
//    Combat { hp, ..default() },
//    SpaceObjectBundle::new(pos, scale, true, Visuals::sprite(sprite)))
// }
pub fn scaled_enemy(pos: Vec3,
                    scale: f32,
                    name: &'static str,
                    thrust: f32,
                    faction: Faction,
                    hp: u32,
                    sprite: MySprite)
                    -> impl Bundle {
  (Name::new(name),
   Navigation::new(thrust),
   NPC { follow_target: None,
         faction },
   Combat { hp,
            is_hostile: true,
            ..default() },
   SpaceObjectBundle::new(pos, scale, true, Visuals::sprite(sprite)))
}
const NORMAL_NPC_SCALE: f32 = 1.9;
const NORMAL_NPC_THRUST: f32 = 400.0;
pub fn normal_sized_npc(pos: Vec3,
                        name: &'static str,
                        thrust: f32,
                        faction: Faction,
                        hp: u32,
                        sprite: MySprite)
                        -> impl Bundle {
  scaled_npc(pos, NORMAL_NPC_SCALE, name, thrust, faction, hp, sprite)
}
// fn space_object_bundle

//   fn new(translation: Vec3, scale: f32, can_move: bool, visuals: Visuals) -> Self {
//     let collider = Collider::sphere(1.0);
//     Self((SpaceObject { scale, ..default() },
//           visuals,
//           LockedAxes::ROTATION_LOCKED,
//           ColliderMassProperties::new(&collider, 1.0),
//           collider,
//           if can_move {
//             RigidBody::Dynamic
//           } else {
//             RigidBody::Static
//           },
//           LinearDamping(1.6),
//           AngularDamping(1.2),
//           LinearVelocity::default(),
//           AngularVelocity::default(),
//           ExternalForce::default().with_persistence(false),
//           ExternalImpulse::default(),
//           SpatialBundle { transform: Transform { translation,
//                                                  rotation: default(),
//                                                  scale: Vec3::splat(scale) },
//                           ..default() }))
//   }
// type TranslationSpawnable2<B1:Bundle,B2> = (B1,fn(Vec3)->B2);
// const HOSTILE_TURRET:TranslationSpawnable2<_,_> = (Name::new("turret"),
//                                                    NPC { follow_target: None,
//                                                          ..default() },
//                                                    Combat { hp: 80,
//                                                             is_hostile: true,
//                                                             ..default() },|pos| SpaceObjectBundle::new(pos,
//                                                                                                        NORMAL_NPC_SCALE,
//                                                                                                        false,
//                                                                                                        Visuals::sprite(MySprite::Turret)));
// static AAA:impl Clone = 5u8;
pub fn hostile_turret(pos: Vec3) -> impl Bundle {
  (Name::new("turret"),
   NPC { follow_target: None,
         ..default() },
   Combat { hp: 80,
            is_hostile: true,
            ..default() },
   SpaceObjectBundle::new(pos,
                          NORMAL_NPC_SCALE,
                          false,
                          Visuals::sprite(MySprite::Turret)))
}
pub fn space_pirate(pos: Vec3) -> impl Bundle {
  (IsHostile(true),
   scaled_enemy(pos,
                NORMAL_NPC_SCALE,
                "space pirate",
                NORMAL_NPC_THRUST,
                Faction::SpacePirates,
                50,
                MySprite::SpaceshipRed))
}
pub fn space_pirate_base(pos: Vec3) -> impl Bundle {
  (Combat { hp: 120,
            is_hostile: false,
            ..default() },
   Interact::SingleOption(InteractSingleOption::Describe),
   name("space pirate base"),
   SpaceObjectBundle::new(pos, 4.0, false, Visuals::sprite(MySprite::SpacePirateBase)))
}
pub fn space_station(pos: Vec3) -> impl Bundle {
  (Combat { hp: 120,
            is_hostile: false,
            ..default() },
   Interact::SingleOption(InteractSingleOption::Describe),
   name("space station"),
   SpaceObjectBundle::new(pos, 4.0, false, Visuals::sprite(MySprite::SpaceStation)))
}
// type TranslationSpawnableF = LazyLock<> fn(Vec3)
pub fn trader(pos: Vec3) -> impl Bundle {
  scaled_npc(pos,
             NORMAL_NPC_SCALE,
             "Trader",
             NORMAL_NPC_THRUST,
             Faction::Traders,
             30,
             MySprite::SpaceshipWhite2)
}
pub fn space_cop(pos: Vec3) -> impl Bundle {
  scaled_npc(pos,
             NORMAL_NPC_SCALE,
             "space cop",
             NORMAL_NPC_THRUST,
             Faction::SpacePolice,
             70,
             MySprite::SpaceshipBlue)
}
pub fn space_wizard(pos: Vec3) -> impl Bundle {
  scaled_npc(pos,
             NORMAL_NPC_SCALE,
             "space wizard",
             NORMAL_NPC_THRUST,
             Faction::SpaceWizards,
             40,
             MySprite::WizardSpaceShip)
}
pub fn nomad(pos: Vec3) -> impl Bundle {
  scaled_npc(pos,
             NORMAL_NPC_SCALE,
             "nomad",
             NORMAL_NPC_THRUST,
             Faction::Wanderers,
             35,
             MySprite::SpaceshipGreen)
}
pub fn alien_soldier(pos: Vec3) -> impl Bundle {
  (IsHostile(true),
   scaled_enemy(pos,
                NORMAL_NPC_SCALE,
                "alien soldier",
                NORMAL_NPC_THRUST,
                Faction::Invaders,
                80,
                MySprite::PurpleEnemyShip))
}
pub fn enemy(pos: Vec3) -> impl Bundle {
  (Enemy,
   Combat::default(),
   scaled_enemy(pos,
                NORMAL_NPC_SCALE,
                "enemy",
                NORMAL_NPC_THRUST,
                Faction::default(),
                50,
                MySprite::PurpleEnemyShip))
}
pub fn npc(pos: Vec3) -> impl Bundle {
  scaled_npc(pos,
             NORMAL_NPC_SCALE,
             "npc",
             NORMAL_NPC_THRUST,
             Faction::default(),
             50,
             MySprite::SpaceshipWhite2)
}
pub fn named_npc(pos: Vec3,npc:NamedNPC) -> impl Bundle {
  let NamedNPC { name, faction, sprite, dialogue_tree } = npc;
  (Name::new(name),
   Interact::MultipleOptions(InteractMultipleOptions::DialogueTree(dialogue_tree)),
   SpaceObjectBundle::new(pos, NORMAL_NPC_SCALE, true, Visuals::sprite(sprite)))
}
pub fn mushroom_man(pos: Vec3) -> impl Bundle {
  (PlayerFollower,
   scaled_npc(pos,
              NORMAL_NPC_SCALE,
              "mushroom man",
              NORMAL_NPC_THRUST,
              Faction::Traders,
              40,
              MySprite::MushroomMan))
}

pub fn sign(pos: Vec3, text: String) -> impl Bundle {
  (Interact::SingleOption(InteractSingleOption::Describe),
   SpaceObjectBundle::new(pos,
                          1.5,
                          false,
                          Visuals::sprite(MySprite::Sign).with_text(text)))
}
pub fn wormhole(pos: Vec3) -> impl Bundle {
  (Interact::SingleOption(InteractSingleOption::Describe),
   name("wormhole"),
   SpaceObjectBundle::new(pos, 4.0, false, Visuals::sprite(MySprite::WormHole)))
}
pub fn asteroid(pos: Vec3) -> impl Bundle {
  (
    // Interact::SingleOption(InteractSingleOption::Asteroid),
    Interact::MultipleOptions(InteractMultipleOptions::AsteroidMiningMinigame{resources_left:5,tool_durability:5}),
    CanBeFollowedByNPC,
    SpaceObjectBundle::new(pos,
      asteroid_scale(),
      false,
      Visuals::sprite(MySprite::Asteroid)))
}

fn item_in_space(image: MySprite,
                 pos: Vec3,
                 scale: f32,
                 name: impl ToString,
                 item_type: Item)
                 -> impl Bundle {
  // let j: Box<dyn SpawnableBundle> = Box::new(image.clone());
  (Name::new(name.to_string()),
   Interact::SingleOption(InteractSingleOption::Item(item_type)),
   SpaceObjectBundle::new(pos, scale, true, Visuals::sprite(image)))
}
fn loot_object(image: MySprite,
               pos: Vec3,
               scale: f32,
               name: impl ToString,
               item_type: Item)
               -> impl Bundle {
  (Name::new(name.to_string()),
   Interact::SingleOption(InteractSingleOption::Item(item_type)),
   SpaceObjectBundle::new(pos, scale, true, Visuals::sprite(image)))
}
fn space_cat(pos: Vec3) -> impl Bundle {
  loot_object(MySprite::SpaceCat,
              pos,
              1.3,
              "space cat".to_string(),
              Item::SpaceCat)
}
fn spaceman(pos: Vec3) -> impl Bundle {
  loot_object(MySprite::SpaceMan,
              pos,
              1.3,
              "spaceman".to_string(),
              Item::Person)
}
// const SPACE_MAN: ToSpawn = from(|pos| {
//   loot_object(MySprite::SpaceMan,
//               pos,
//               1.3,
//               "spaceman".to_string(),
//               Item::Person)
// });
fn space_coin(pos: Vec3) -> impl Bundle {
  loot_object(MySprite::Coin,
              pos,
              1.7,
              "space coin".to_string(),
              Item::SpaceCoin)
}
pub fn ice_asteroid(pos: Vec3) -> impl Bundle {
  loot_object(MySprite::IceAsteroid,
              pos,
              asteroid_scale(),
              "ice",
              Item::DiHydrogenMonoxide)
}
pub fn crystal_asteroid(pos: Vec3) -> impl Bundle {
  loot_object(MySprite::CrystalAsteroid,
              pos,
              asteroid_scale(),
              "crystal asteroid",
              Item::Crystal)
}
pub fn crystal_monster(pos: Vec3) -> impl Bundle {
  (name("crystal monster"),
   // Interact::Item(item_type),
   SpaceObjectBundle::new(pos, 2.1, true, Visuals::sprite(MySprite::CrystalMonster)))
}
fn container(translation: Vec3,
             contents: impl IntoIterator<Item = (Item, u32)>)
             -> impl Bundle {
  (name("container"),
   // Inventory::from_contents(contents),
   Interact::SingleOption(InteractSingleOption::Container(vec(contents))),
   SpaceObjectBundle::new(translation, 2.1, true, Visuals::sprite(MySprite::Container)))
}
pub fn hp_box(pos: Vec3) -> impl Bundle {
  (name("hp box"),
   Interact::SingleOption(InteractSingleOption::HPBox),
   SpaceObjectBundle::new(pos, 0.9, true, Visuals::sprite(MySprite::HPBox)))
}

fn treasurecontainer(pos: Vec3) -> impl Bundle {
  container(pos, [(Item::SpaceCoin, 4), (Item::Coffee, 1)])
}
fn crystalmonster(pos: Vec3) -> impl Bundle {
  (name("crystal monster"),
   Interact::SingleOption(InteractSingleOption::Describe),
   SpaceObjectBundle::new(pos, 1.7, true, Visuals::sprite(MySprite::CrystalMonster)))
}
fn spawncrystalmonster(pos: Vec3,mut c:&mut Commands) {
  c.spawn((name("crystal monster"),
           Interact::SingleOption(InteractSingleOption::Describe),
           SpaceObjectBundle::new(pos, 1.7, true, Visuals::sprite(MySprite::CrystalMonster))));
}
fn sphericalcow(pos: Vec3) -> impl Bundle {
  (name("spherical cow"),
   Interact::MultipleOptions(InteractMultipleOptions::DialogueTree(SPHERICAL_SPACE_COW)),
   SpaceObjectBundle::new(pos, 1.7, true, Visuals::sprite(MySprite::SphericalCow)))
}

const fn lazy_translation_spawnable<B:Bundle>(f:fn (Vec3)->B)->LazyLock<TranslationSpawnable>{
  LazyLock::new(|| TranslationSpawnable::from(f))
}
// static SPHERICAL_COW:LazyLock<TranslationSpawnable> =
//   lazy_translation_spawnable(|pos:Vec3|
//                              (name("spherical cow"),
//                               Interact::MultipleOptions(InteractMultipleOptions::DialogueTree(SPHERICAL_SPACE_COW)),
//                               SpaceObjectBundle::new(pos, 1.7, true, Visuals::sprite(MySprite::SphericalCow))));
lazy_static!{
     pub static ref SPHERICAL_COW: TranslationSpawnable =
    (|pos:Vec3|
     (name("spherical cow"),
      Interact::MultipleOptions(InteractMultipleOptions::DialogueTree(SPHERICAL_SPACE_COW)),
      SpaceObjectBundle::new(pos, 1.7, true, Visuals::sprite(MySprite::SphericalCow)))).into();
}
use lazy_static::lazy_static;

lazy_static! {
  pub static ref HOSTILE_TURRET: TranslationSpawnable = from(|pos: Vec3| {
    (Name::new("turret"),
     NPC { follow_target: None, ..default() },
     Combat { hp: 80, is_hostile: true, ..default() },
     SpaceObjectBundle::new(pos, NORMAL_NPC_SCALE, false, Visuals::sprite(MySprite::Turret)))});
  pub static ref SPACE_PIRATE: TranslationSpawnable = from(|pos: Vec3| {
    (IsHostile(true),
     scaled_enemy(pos, NORMAL_NPC_SCALE, "space pirate", NORMAL_NPC_THRUST, Faction::SpacePirates, 50, MySprite::SpaceshipRed))});
  pub static ref SPACE_PIRATE_BASE: TranslationSpawnable = from(|pos: Vec3| {
    (Combat { hp: 120, is_hostile: false, ..default() },
     Interact::SingleOption(InteractSingleOption::Describe),
     name("space pirate base"),
     SpaceObjectBundle::new(pos, 4.0, false, Visuals::sprite(MySprite::SpacePirateBase)))});
  pub static ref SPACE_STATION: TranslationSpawnable = from(|pos: Vec3| {
    (Combat { hp: 120, is_hostile: false, ..default() },
     Interact::SingleOption(InteractSingleOption::Describe),
     name("space station"),
     SpaceObjectBundle::new(pos, 4.0, false, Visuals::sprite(MySprite::SpaceStation)))});
  pub static ref TRADER: TranslationSpawnable = from(|pos: Vec3| {
    scaled_npc(pos, NORMAL_NPC_SCALE, "Trader", NORMAL_NPC_THRUST, Faction::Traders, 30, MySprite::SpaceshipWhite2)});
  pub static ref SPACE_COP: TranslationSpawnable = from(|pos: Vec3| {
    scaled_npc(pos, NORMAL_NPC_SCALE, "space cop", NORMAL_NPC_THRUST, Faction::SpacePolice, 70, MySprite::SpaceshipBlue)});
  pub static ref SPACE_WIZARD: TranslationSpawnable = from(|pos: Vec3| {
    scaled_npc(pos, NORMAL_NPC_SCALE, "space wizard", NORMAL_NPC_THRUST, Faction::SpaceWizards, 40, MySprite::WizardSpaceShip)});
  pub static ref NOMAD: TranslationSpawnable = from(|pos: Vec3| {
    scaled_npc(pos, NORMAL_NPC_SCALE, "nomad", NORMAL_NPC_THRUST, Faction::Wanderers, 35, MySprite::SpaceshipGreen)});
  pub static ref ALIEN_SOLDIER: TranslationSpawnable = from(|pos: Vec3| {
    (IsHostile(true),
     scaled_enemy(pos, NORMAL_NPC_SCALE, "alien soldier", NORMAL_NPC_THRUST, Faction::Invaders, 80, MySprite::PurpleEnemyShip))});
  pub static ref ENEMY: TranslationSpawnable = from(|pos: Vec3| {
    (Enemy,
     Combat::default(),
     scaled_enemy(pos, NORMAL_NPC_SCALE, "enemy", NORMAL_NPC_THRUST, Faction::default(), 50, MySprite::PurpleEnemyShip))});
  pub static ref NPC: TranslationSpawnable = from(|pos: Vec3| {
    scaled_npc(pos, NORMAL_NPC_SCALE, "npc", NORMAL_NPC_THRUST, Faction::default(), 50, MySprite::SpaceshipWhite2)});
  pub static ref MUSHROOM_MAN: TranslationSpawnable = from(|pos: Vec3| {
    (PlayerFollower,
     scaled_npc(pos, NORMAL_NPC_SCALE, "mushroom man", NORMAL_NPC_THRUST, Faction::Traders, 40, MySprite::MushroomMan))});
  pub static ref WORMHOLE: TranslationSpawnable = from(|pos: Vec3| {
    (Interact::SingleOption(InteractSingleOption::Describe),
     name("wormhole"),
     SpaceObjectBundle::new(pos, 4.0, false, Visuals::sprite(MySprite::WormHole)))});
  pub static ref ASTEROID: TranslationSpawnable = from(|pos: Vec3| {
    (Interact::MultipleOptions(InteractMultipleOptions::AsteroidMiningMinigame{resources_left:5,tool_durability:5}),
     CanBeFollowedByNPC,
     SpaceObjectBundle::new(pos, asteroid_scale(), false, Visuals::sprite(MySprite::Asteroid)))});
  pub static ref SPACE_CAT: TranslationSpawnable = from(|pos: Vec3| {
    loot_object(MySprite::SpaceCat, pos, 1.3, "space cat".to_string(), Item::SpaceCat)});
  pub static ref SPACEMAN: TranslationSpawnable = from(|pos: Vec3| {
    loot_object(MySprite::SpaceMan, pos, 1.3, "spaceman".to_string(), Item::Person)});
  pub static ref SPACE_COIN: TranslationSpawnable = from(|pos: Vec3| {
    loot_object(MySprite::Coin, pos, 1.7, "space coin".to_string(), Item::SpaceCoin)});
  pub static ref ICE_ASTEROID: TranslationSpawnable = from(|pos: Vec3| {
    loot_object(MySprite::IceAsteroid, pos, asteroid_scale(), "ice".to_string(), Item::DiHydrogenMonoxide)});
  pub static ref CRYSTAL_ASTEROID: TranslationSpawnable = from(|pos: Vec3| {
    loot_object(MySprite::CrystalAsteroid, pos, asteroid_scale(), "crystal asteroid".to_string(), Item::Crystal)});
  pub static ref CRYSTAL_MONSTER: TranslationSpawnable = from(|pos: Vec3| {
    (name("crystal monster"),
     SpaceObjectBundle::new(pos, 2.1, true, Visuals::sprite(MySprite::CrystalMonster)))});
  pub static ref HP_BOX: TranslationSpawnable = from(|pos: Vec3| {
    (name("hp box"),
     Interact::SingleOption(InteractSingleOption::HPBox),
     SpaceObjectBundle::new(pos, 0.9, true, Visuals::sprite(MySprite::HPBox)))});
  pub static ref TREASURE_CONTAINER: TranslationSpawnable = from(|pos: Vec3| {
    container(pos, [(Item::SpaceCoin, 4), (Item::Coffee, 1)])});
  pub static ref ZORP: TranslationSpawnable = from(|pos: Vec3| {
    (name("zorp"),
     Interact::MultipleOptions(InteractMultipleOptions::DialogueTree(ZORP)),
     SpaceObjectBundle::new(pos, 1.7, true, Visuals::sprite(MySprite::Zorp)))});
  pub static ref TRADE_STATION: TranslationSpawnable = from(|pos: Vec3| {
    let (trade, text) = if prob(0.5) {
      let trade_buy = pick([Item::DiHydrogenMonoxide, Item::Crystal, Item::SpaceCat]).unwrap();
      (Interact::SingleOption(InteractSingleOption::Trade { inputs: (trade_buy, 1), outputs: (Item::SpaceCoin, 5) }),
       format!("space station\nbuys {:?}", trade_buy))
    } else {
      let trade_sell = pick([Item::Spice, Item::Coffee, Item::Rock]).unwrap();
      (Interact::SingleOption(InteractSingleOption::Trade { inputs: (Item::SpaceCoin, 5), outputs: (trade_sell, 1) }),
       format!("space station\nsells {:?}", trade_sell))
    };
    (name("space station"),
     CanBeFollowedByNPC,
     trade,
     SpaceObjectBundle::new(pos, 3.0, false, Visuals::sprite(MySprite::SpaceStation).with_text(text)))});
  pub static ref FLOATING_ISLAND: TranslationSpawnable = from(|pos: Vec3| {
    (name("floating island"),
     Interact::SingleOption(InteractSingleOption::Describe),
     SpaceObjectBundle::new(pos, 3.4, false, Visuals::sprite(MySprite::FloatingIsland)))});
  pub static ref ABANDONED_SHIP: TranslationSpawnable = from(|pos: Vec3| {
    (name("abandoned ship"),
     Interact::MultipleOptions(InteractMultipleOptions::Salvage { how_much_loot: 3 }),
     SpaceObjectBundle::new(pos, 2.0, false, Visuals::sprite(MySprite::SpaceshipAbandoned)))});
}
fn zorp(pos: Vec3) -> impl Bundle {
{
  let k: ABANDONED_SHIP = ABANDONED_SHIP;
  k.
}
  (name("zorp"),
   Interact::MultipleOptions(InteractMultipleOptions::DialogueTree(ZORP)),
   SpaceObjectBundle::new(pos, 1.7, true, Visuals::sprite(MySprite::Zorp)))
}
fn tradestation(pos: Vec3) -> impl Bundle {
  let (trade, text) = if prob(0.5) {
    let trade_buy = pick([Item::DiHydrogenMonoxide, Item::Crystal, Item::SpaceCat]).unwrap();

    (Interact::SingleOption(InteractSingleOption::Trade { inputs: (trade_buy, 1),
                                                          outputs: (Item::SpaceCoin, 5) }),
     format!("space station\nbuys {:?}", trade_buy))
  } else {
    let trade_sell = pick([Item::Spice, Item::Coffee, Item::Rock]).unwrap();
    (Interact::SingleOption(InteractSingleOption::Trade { inputs: (Item::SpaceCoin, 5),
                                                          outputs: (trade_sell, 1) }),
     format!("space station\nsells {:?}", trade_sell))
  };
  (name("space station"),
   CanBeFollowedByNPC,
   trade,
   SpaceObjectBundle::new(pos,
                          3.0,
                          false,
                          Visuals::sprite(MySprite::SpaceStation).with_text(text)))
}
fn floatingisland(pos: Vec3) -> impl Bundle {
  (name("floating island"),
   Interact::SingleOption(InteractSingleOption::Describe),
   SpaceObjectBundle::new(pos, 3.4, false, Visuals::sprite(MySprite::FloatingIsland)))
}

// fn gate(pos: Vec3) -> impl Bundle {
//   (name("gate"),
//    Interact::SingleOption(InteractSingleOption::Gate),
//    Gate,
//    SpaceObjectBundle::new(pos,
//                           2.1,
//                           false,
//                           Visuals::sprite(MySprite::Gate)
//                           .with_text(format!("warp gate to {}",random_zone_name()) )))
// }
fn abandonedship(pos: Vec3) -> impl Bundle {
  (name("abandoned ship"),
   Interact::MultipleOptions(InteractMultipleOptions::Salvage { how_much_loot: 3 }),
   SpaceObjectBundle::new(pos,
                          2.0,
                          false,
                          Visuals::sprite(MySprite::SpaceshipAbandoned)))
}

#[derive(Component, Clone, Debug, Default)]
pub struct Inventory(HashMap<Item, u32>);

impl Inventory {
  fn add_contents(&mut self, contents: impl IntoIterator<Item = (Item, u32)>) {
    for (item, n) in contents {
      *(self.0.entry(item).or_default()) += n;
    }
  }
  fn trade(&mut self,
           inputs: impl IntoIterator<Item = (Item, u32)>,
           outputs: impl IntoIterator<Item = (Item, u32)>) {
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Item {
  SpaceCat,
  Person,
  Spice,
  Coffee,
  SpaceCoin,
  Crystal,
  DiHydrogenMonoxide,
  Rock,
  SpaceMinerals
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

#[derive(Clone)]
enum InteractMultipleOptions {
  Salvage {
    how_much_loot: u8
  },
  DialogueTree(DialogueTree),
  AsteroidMiningMinigame {
    resources_left: u8,
    tool_durability: u8
  }
}
impl InteractMultipleOptions {
  fn interact(self) -> (String, Vec<(String, MyCommand, Self)>) {
    match self {
      // InteractMultipleOptions::AlienMerchantDialogue { node } => {
      //   let msg =
      //     "You encounter Zorp, an alien merchant with tentacles and three eyes.".to_string();
      //   let options = mapv(|(node, player_say, merchant_say)| {
      //                        (player_say.to_string(),
      //                         MyCommand::message_add(merchant_say),
      //                         InteractMultipleOptions::AlienMerchantDialogue { node })
      //                      },
      //                      node.options());
      //   (msg, options)
      // }
      InteractMultipleOptions::AsteroidMiningMinigame { resources_left,
                                                        tool_durability } => {
        let msg =
          format!("You're mining an asteroid. Resources left: {}. Tool durability: {}.",
                  resources_left, tool_durability);
        let mut options = vec![];

        if resources_left > 0 && tool_durability > 0 {
          options.push((
            "Mine carefully".to_string(),
            MyCommand::multi([
              MyCommand::message_add("You mine carefully, preserving your tool."),
              MyCommand::give_item_to_player(Item::SpaceMinerals),
            ]),
            Self::AsteroidMiningMinigame { resources_left: resources_left - 1, tool_durability },
          ));
          options.push((
            "Mine aggressively".to_string(),
            MyCommand::multi([
              MyCommand::message_add("You mine aggressively, risking your tool for more resources."),
              MyCommand::give_item_to_player(Item::SpaceMinerals),
              MyCommand::give_item_to_player(Item::SpaceMinerals),
            ]),
            Self::AsteroidMiningMinigame { resources_left: resources_left - 1, tool_durability: tool_durability - 1 },
          ));
        }

        options.push(("Leave asteroid".to_string(),
                      MyCommand::end_object_interaction_mini_game(),
                      self.clone()));

        (msg, options)
      }
      InteractMultipleOptions::Salvage { how_much_loot } => {
        let msg = "It's a destroyed spaceship. Maybe you can find loot in it".to_string();
        let options = if how_much_loot > 0 {
          vec![("take some".to_string(),
                MyCommand::multi([MyCommand::message_add("You found loot"),
                                  MyCommand::give_item_to_player(Item::SpaceCoin)]),
                Self::Salvage { how_much_loot: how_much_loot - 1 }),
               ("don't take".to_string(), MyCommand::none(), self.clone()),
               ("leave".to_string(),
                MyCommand::end_object_interaction_mini_game(),
                self.clone()),]
        } else {
          vec![("leave".to_string(),
                MyCommand::end_object_interaction_mini_game(),
                self.clone()),]
        };
        (msg, options)
      }
      // InteractMultipleOptions::SphericalCowDialogueTree { node } => {
      //   let msg = "It's a spherical cow in a vacuum".to_string();
      //   let options = node.options()
      //                     .into_iter()
      //                     .map(|(node, playersay, cowsay)| {
      //                       (playersay.to_string(),
      //                        MyCommand::message_add(cowsay),
      //                        InteractMultipleOptions::SphericalCowDialogueTree { node })
      //                     })
      //                     .collect();
      //   (msg, options)
      // }
    }
  }
}

#[derive(Clone)]
enum InteractSingleOption {
  Message(String),
  // Salvage { how_much_loot: u8 },
  Asteroid,
  HPBox,
  Describe,
  Item(Item),
  Trade {
    inputs: (Item, u32),
    outputs: (Item, u32)
  },
  Gate(Vec3),
  Container(Vec<(Item, u32)>)
}

impl InteractSingleOption {
  fn interact(self,
              self_entity: Entity,
              self_name: String,
              player_inventory: &Inventory)
              -> (String, MyCommand) {
    match self {
      InteractSingleOption::Message(m) => ("examine".to_string(), MyCommand::message_add(m)),
      InteractSingleOption::Asteroid => {
        (format!("examine {self_name}"),
         MyCommand::message_add("it's an asteroid"))
      }
      InteractSingleOption::HPBox => {
        ("take hp box".to_string(),
         MyCommand::multi([MyCommand::update_player_component(|combat: Combat| {
                             Combat { hp: combat.hp + 50,
                                      ..combat }
                           }),
                           MyCommand::despawn(self_entity)]))
      }
      InteractSingleOption::Describe => {
        (format!("examine {self_name}"), MyCommand::message_add(self_name))
      }
      InteractSingleOption::Item(item) => {
        (format!("take {self_name}"),
         MyCommand::multi([MyCommand::despawn(self_entity),
                           MyCommand::message_add(format!("You got a {}",debugfmt(item)) ),
                           MyCommand::give_item_to_player(item)]))
      }

      InteractSingleOption::Trade { inputs: (input_item, input_number),
                                    outputs: (output_item, output_number) } => {
        ("trade".to_string(),
         if let Some(&n) = player_inventory.0.get(&input_item)
            && n >= input_number
         {
           MyCommand::multi([
             MyCommand::mutate_player_component(move |mut inventory:&mut Inventory|{
               inventory.trade([(input_item, input_number)],[(output_item, output_number)]);
             }),
             MyCommand::message_add(format!("You traded {:?} {:?} for {:?} {:?}s",
                                           input_number,
                                           input_item,
                                           output_number,
                                           output_item))
           ])
         } else {
           MyCommand::message_add("You don't have enough items")
         })
      }
      InteractSingleOption::Gate(destination_pos) => {
        ("interact".to_string(),
         MyCommand::update_player_component(move |transform| Transform { translation:
                                                                           destination_pos,
                                                                         ..transform }))
      }
      InteractSingleOption::Container(items) => {
        ("take container".to_string(),
         MyCommand::multi([MyCommand::despawn(self_entity),
                           MyCommand::message_add("you got things"),
                           MyCommand::mutate_player_component(|mut inventory: &mut Inventory| {
                             inventory.add_contents(items);
                           })]))
      }
    }
  }
}

#[derive(Component, Clone)]
enum Interact {
  SingleOption(InteractSingleOption),
  MultipleOptions(InteractMultipleOptions)
}
const INTERACTION_RANGE: f32 = 8.0;
fn interact(mut playerq: Query<(Entity, &mut Transform, &Combat, &Inventory),
                  With<Player>>,
            mut interactable_q: Query<(Entity, &Transform, &mut Interact, Option<&Name>),
                  Without<Player>>,
            gate_q: Query<(&GlobalTransform, &Gate)>,
            mut c: Commands,
            keys: Res<ButtonInput<KeyCode>>,
            mut ui_data: ResMut<UIData>) {
  ui_data.interact_message = None;
  if let Ok((player, player_transform, player_combat, player_inventory)) =
    playerq.get_single_mut()
     && let player_pos = player_transform.translation
     && let closest_interactable_thing =
       filter_least(|tup| {
                      let dist = tup.1.translation.distance(player_pos);
                      (dist < INTERACTION_RANGE).then_some(dist as u32)
                    },
                    &mut interactable_q)
     && let Some((interact_entity, transform, mut interact, oname)) =
       closest_interactable_thing
  {
    match interact.as_mut() {
      Interact::SingleOption(interact_single_option) => {
        let (message, command) =
          interact_single_option.clone()
                                .interact(interact_entity, namefmt(oname), player_inventory);
        ui_data.interact_message = Some(format!("[SPACE: {message}]"));
        if keys.just_pressed(KeyCode::Space) {
          c.add(command);
        }
      }
      Interact::MultipleOptions(interact_multiple_options) => {
        let (msg, options) = interact_multiple_options.clone().interact();
        ui_data.interact_message =
          Some(intersperse_newline([msg, default()].into_iter()
                                                   .chain((&options).into_iter()
                                                                    .enumerate()
                                                                    .map(|(n, tup)| {
                                                                      format!("{}: {}",
                                                                              n + 1,
                                                                              tup.0)
                                                                    }))));
        let number_picked =
          find_map(|(n, key): (u8, KeyCode)| keys.just_pressed(key).then_some(n),
                   [(0, KeyCode::Digit0),
                    (1u8, KeyCode::Digit1),
                    (2, KeyCode::Digit2),
                    (3, KeyCode::Digit3),
                    (4, KeyCode::Digit4),
                    (5, KeyCode::Digit5),
                    (6, KeyCode::Digit6),
                    (7, KeyCode::Digit7),
                    (8, KeyCode::Digit8),
                    (9, KeyCode::Digit9)]);
        for (n, (string, command, new_interact)) in options.into_iter().enumerate() {
          if number_picked == Some(n as u8 + 1) {
            c.add(command);
            *interact_multiple_options = new_interact.clone();
          }
        }
      }
    }
  }
}

// use bevy::prelude::*;

// const INTERACTION_RANGE: f32 = 8.0;

fn interact2(
    mut playerq: Query<(Entity, &mut Transform, &Combat, &Inventory), With<Player>>,
    mut interactable_q: Query<(Entity, &Transform, &mut Interact, Option<&Name>), Without<Player>>,
    gate_q: Query<(&GlobalTransform, &Gate)>,
    mut c: Commands,
    keys: Res<ButtonInput<KeyCode>>,
    mut ui_data: ResMut<UIData>
) {
  ui_data.interact_message = None;
  if let Ok((player, player_transform, player_combat, player_inventory)) = playerq.get_single_mut() {
    let player_pos = player_transform.translation;
    if let Some((interact_entity, transform, mut interact, oname)) = filter_least(
      |tup| {
        let dist = tup.1.translation.distance(player_pos);
        (dist < INTERACTION_RANGE).then_some(dist as u32)
      },
      &mut interactable_q
    ) {
      match interact.as_mut() {
        Interact::SingleOption(interact_single_option) => {
          let (message, command) = match interact_single_option.clone() {
            InteractSingleOption::Message(m) => ("examine".to_string(), MyCommand::message_add(m)),
            InteractSingleOption::Asteroid => (
              format!("examine {}", namefmt(oname)),
              MyCommand::message_add("it's an asteroid")
            ),
            InteractSingleOption::HPBox => (
              "take hp box".to_string(),
              MyCommand::multi([
                MyCommand::update_player_component(|combat: Combat| Combat { hp: combat.hp + 50, ..combat }),
                MyCommand::despawn(interact_entity)
              ])
            ),
            InteractSingleOption::Describe => (
              format!("examine {}", namefmt(oname)),
              MyCommand::message_add(namefmt(oname))
            ),
            InteractSingleOption::Item(item) => (
              format!("take {}", namefmt(oname)),
              MyCommand::multi([
                MyCommand::despawn(interact_entity),
                MyCommand::message_add(format!("You got a {}", debugfmt(item))),
                MyCommand::give_item_to_player(item)
              ])
            ),
            InteractSingleOption::Trade { inputs: (input_item, input_number), outputs: (output_item, output_number) } => (
              "trade".to_string(),
              if let Some(&n) = player_inventory.0.get(&input_item) {
                if n >= input_number {
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
              } else {
                MyCommand::message_add("You don't have the required item")
              }
            ),
            InteractSingleOption::Gate(destination_pos) => (
              "interact".to_string(),
              MyCommand::update_player_component(move |transform| Transform {
                translation: destination_pos,
                ..transform
              })
            ),
            InteractSingleOption::Container(items) => (
              "take container".to_string(),
              MyCommand::multi([
                MyCommand::despawn(interact_entity),
                MyCommand::message_add("you got things"),
                MyCommand::mutate_player_component(|mut inventory: &mut Inventory| {
                  inventory.add_contents(items);
                })
              ])
            ),
          };

          ui_data.interact_message = Some(format!("[SPACE: {message}]"));
          if keys.just_pressed(KeyCode::Space) {
            c.add(command);
          }
        },
        Interact::MultipleOptions(interact_multiple_options) => {
          let (msg, options) = match interact_multiple_options.clone() {
            // InteractMultipleOptions::AlienMerchantDialogue { node } => {
            //   let msg = "You encounter Zorp, an alien merchant with tentacles and three eyes.".to_string();
            //   let options = mapv(
            //     |(node, player_say, merchant_say)| (
            //       player_say.to_string(),
            //       MyCommand::message_add(merchant_say),
            //       InteractMultipleOptions::AlienMerchantDialogue { node }
            //     ),
            //     node.options()
            //   );
            //   (msg, options)
            // },
            InteractMultipleOptions::AsteroidMiningMinigame { resources_left, tool_durability } => {
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
                    MyCommand::give_item_to_player(Item::SpaceMinerals),
                  ]),
                  InteractMultipleOptions::AsteroidMiningMinigame {
                    resources_left: resources_left - 1,
                    tool_durability,
                  },
                ));
                options.push((
                  "Mine aggressively".to_string(),
                  MyCommand::multi([
                    MyCommand::message_add("You mine aggressively, risking your tool for more resources."),
                    MyCommand::give_item_to_player(Item::SpaceMinerals),
                    MyCommand::give_item_to_player(Item::SpaceMinerals),
                  ]),
                  InteractMultipleOptions::AsteroidMiningMinigame {
                    resources_left: resources_left - 1,
                    tool_durability: tool_durability - 1,
                  },
                ));
              }

              options.push((
                "Leave asteroid".to_string(),
                MyCommand::end_object_interaction_mini_game(),
                interact_multiple_options.clone(),
              ));

              (msg, options)
            },
            InteractMultipleOptions::Salvage { how_much_loot } => {
              // set_msg("It's a destroyed spaceship. Maybe you can find loot in it".to_string());
              let msg = "It's a destroyed spaceship. Maybe you can find loot in it".to_string();
              let options = if how_much_loot > 0 {
                vec![
                  ("take some".to_string(),
                   MyCommand::multi([
                     MyCommand::message_add("You found loot"),
                     MyCommand::give_item_to_player(Item::SpaceCoin),
                   ]),
                   InteractMultipleOptions::Salvage { how_much_loot: how_much_loot - 1 },),
                  ("don't take".to_string(),
                   MyCommand::none(),
                   interact_multiple_options.clone(),),
                  ("leave".to_string(),
                   MyCommand::end_object_interaction_mini_game(),
                   interact_multiple_options.clone(),),
                ]
              } else {
                vec![("leave".to_string(),
                      MyCommand::end_object_interaction_mini_game(),
                      interact_multiple_options.clone(),),]
              };
              (msg, options)
            },
            // InteractMultipleOptions::SphericalCowDialogueTree { node } => {
            //   let msg = "It's a spherical cow in a vacuum".to_string();
            //   let options = mapv(|(node, playersay, cowsay)| (
            //     playersay.to_string(),
            //     MyCommand::message_add(cowsay),
            //     InteractMultipleOptions::SphericalCowDialogueTree { node }
            //   ),node .options()) ;
            //   (msg, options)
            // },
          };

          ui_data.interact_message = Some(intersperse_newline(
            [msg, default()].into_iter().chain(
              (&options).into_iter().enumerate().map(|(n, tup)| format!("{}: {}", n + 1, tup.0)))));

          let number_picked = find_map(
            |(n, key): (u8, KeyCode)| keys.just_pressed(key).then_some(n),
            [(0, KeyCode::Digit0),
             (1u8, KeyCode::Digit1),
             (2, KeyCode::Digit2),
             (3, KeyCode::Digit3),
             (4, KeyCode::Digit4),
             (5, KeyCode::Digit5),
             (6, KeyCode::Digit6),
             (7, KeyCode::Digit7),
             (8, KeyCode::Digit8),
             (9, KeyCode::Digit9)]);
          for (n, (string, command, new_interact)) in options.into_iter().enumerate() {
            if number_picked == Some(n as u8 + 1) {
              c.add(command);
              *interact_multiple_options = new_interact.clone();
            }
          }
        },
      }
    }
  }
}

fn namefmt(oname: Option<&Name>) -> String {
  match oname {
    Some(name) => name.to_string(),
    None => "unnamed entity".to_string()
  }
}
fn lazy<T, F: FnOnce() -> T>(f: F) -> LazyCell<T, F> { LazyCell::new(f) }


fn signal_strength(player_pos: Vec3, pos: Vec3, scale: f32) -> f32 {
  scale.powi(2) / (player_pos.distance(pos)).powi(2)
}
const MESSAGE_SHOW_TIME_TICKS: u32 = 600;
fn ui(mut c: Commands,
      camq: Query<(Entity, &GlobalTransform), With<Camera3d>>,
      playerq: Query<(Entity, &Player, &GlobalTransform, &Combat, &Inventory)>,
      target_q: Query<(Entity,
             &Transform,
             &SpaceObject,
             Option<&Name>,
             Option<&Combat>,
             Option<&Planet>)>,
      mut ui_data: ResMut<UIData>,
      time: Res<TimeTicks>,
      view_root_q: Query<Entity, With<ViewRoot>>) {
  struct TargetData<'t> {
    entity: Entity,
    transform: &'t Transform,
    distance: f32,
    signal_strength: f32,
    spaceobject: &'t SpaceObject,
    oname: Option<&'t Name>,
    name: String,
    ocombat: Option<&'t Combat>,
    oplanet: Option<&'t Planet>
  }
  if let (Ok((_, player, player_globaltransform, player_combat, player_inventory)),
          Ok((camera, _))) = (playerq.get_single(), camq.get_single())
  {
    let player_pos = player_globaltransform.translation();
    let get_target_data = |e: Entity| {
      target_q.get(e)
              .map(|(entity, transform, spaceobject, oname, ocombat, oplanet)| {
                     let distance = player_pos.distance(transform.translation);
                     let name = namefmt(oname);
                     TargetData { entity,
                                  transform,
                                  spaceobject,
                                  oname,
                                  name,
                                  ocombat,
                                  oplanet,
                                  distance,
                                  signal_strength: 1000.0 * transform.scale.x.powi(2)
                                                   / distance.powi(2) }
                   })
              .ok()
    };
    // let overview_max_len = 15;
    // let overview_data =
    //   mapv(|TargetData { distance, name, .. }| format!("{name} d.:{:.1}", distance),
    //        take(overview_max_len,
    //             sort_by_key(|TargetData { distance, .. }| *distance as u32,
    //                         filter_map(|tup| get_target_data(tup.0), &target_q))));
    let overview_data =
      mapv(|(name, hp, distance)| format!("{name} hp:{hp} <->{:.1}", distance),
           sort_by_key(|(_, _, distance)| *distance as u32,
                       filter_map(|tup| match get_target_data(tup.0) {
                                    Some(TargetData { distance,
                                                      name,
                                                      ocombat:
                                                        Some(Combat { hp,
                                                                      is_hostile:
                                                                        true,
                                                                      .. }),
                                                      .. })
                                      if distance < COMBAT_RANGE =>
                                    {
                                      Some((name, hp, distance))
                                    }
                                    _ => None
                                  },
                                  &target_q)));
    let target_data = if let Some(player_target) = player.target()
                         && let Some(TargetData { distance,
                                                  name,
                                                  ocombat,
                                                  oplanet,
                                                  .. }) = get_target_data(player_target)
    {
      let somestring = |x| Some(string(x));
      [Some(format!("Target: {name}")),
       oplanet.map(rust_utils::prettyfmt),
       ocombat.map(|&Combat { hp, .. }| format!("hp: {hp}")),
       Some(format!("Distance: {:.1}", distance)),
       somestring("q: approach"),
       somestring("l: shoot laser"),
       somestring("r: toggle shoot"),
       somestring("x: untarget")].into_iter()
                                 .flatten()
                                 .collect()
    } else {
      default()
    };
    let player_inventory = player_inventory.clone();
    let infobox_data =
      map(ToString::to_string,
          [format!("{:.1}", player_pos).as_str(),
           format!("hp: {}", player_combat.hp).as_str(),
           format!("energy: {}", player_combat.energy).as_str(),
           "w,a,s,d,shift,ctrl: move",
           "z: spawn mushroom man",
           "q: toggle shield",
           "t: target nearest hostile",
           "g: warp",
           "you have:"]).chain(map(|(item, n)| format!("{} {:?}s", n, item),
                                   player_inventory.0.clone()))
                        .collect();

    let current_time = time.0;
    let current_time_ticks = current_time;
    let message_log = rust_utils::filterv(|Message { string, time }| {
                                            time + MESSAGE_SHOW_TIME_TICKS > current_time
                                          },
                                          ui_data.message_log.clone());

    let old_ui_data = ui_data.clone();
    *ui_data = UIData { target_data,
                        overview_data,
                        current_time_ticks,
                        message_log,
                        infobox_data,
                        ..old_ui_data };
    // .as_mut().update(|old_ui_data| UIData { target_data,
    //                                              overview_data,
    //                                              current_time_ticks,
    //                                              message_log,
    //                                              infobox_data,
    //                                              ..old_ui_data });

    if view_root_q.is_empty() {
      ui_data.message_add("message1");
      ui_data.message_add("message2");
      ui_data.message_add("message3");
      c.spawn(UIMainView.to_root());

      c.spawn(ui_root_thing_in_the_world());
    }
  }
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
fn npc_movement(mut npc_q: Query<(&mut NPC, &mut Navigation, &GlobalTransform)>,
                follow_target_q: Query<(Entity, &GlobalTransform),
                      With<CanBeFollowedByNPC>>) {
  let get_target_pos = |e| {
    follow_target_q.get(e)
                   .ok()
                   .map(|(_, globaltransform)| globaltransform.translation())
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

#[derive(Clone, Copy, Debug)]
pub enum PlanetType {
  MarsLikePlanet,
  HabitablePlanet,
  SandPlanet,
  IcePlanet,
  LavaPlanet,
  BrownGasGiant
}
#[derive(Clone, Component, Debug)]
struct Planet {
  pub planet_type: PlanetType,
  pub population: u32
}

#[derive(Clone, Copy, Assoc)]
#[func(pub fn to_spawn(&self) -> Option<TranslationSpawnable>)]
enum SpawnableTemplate {
  Probabilistic(&'static [(f32, SpawnableTemplate)]),
  #[assoc(to_spawn = treasurecontainer.into())]
  TreasureContainer,
  #[assoc(to_spawn = hostile_turret.into())]
  HostileTurret,
  #[assoc(to_spawn = TranslationSpawnable::multi([(Vec3::Y * 4.0,hostile_turret.into()),(Vec3::ZERO,hostile_turret.into()),(Vec3::NEG_Y * 4.0,hostile_turret.into())]))]
  ThreeHostileTurrets,
  #[assoc(to_spawn = space_pirate.into())]
  SpacePirate,
  #[assoc(to_spawn = crystalmonster.into())]
  CrystalMonster,
  #[assoc(to_spawn = spaceman.into())]
  SpaceMan,
  #[assoc(to_spawn = wormhole.into())]
  WormHole,
  #[assoc(to_spawn = npc.into())]
  NPC,
  #[assoc(to_spawn = hp_box.into())]
  HPBox,
  #[assoc(to_spawn = space_coin.into())]
  SpaceCoin,
  #[assoc(to_spawn = tradestation.into())]
  TradeStation,
  #[assoc(to_spawn = space_pirate_base.into())]
  SpacePirateBase,
  #[assoc(to_spawn = asteroid.into())]
  Asteroid,
  #[assoc(to_spawn = ice_asteroid.into())]
  IceAsteroid,
  #[assoc(to_spawn = crystal_asteroid.into())]
  CrystalAsteroid,
  #[assoc(to_spawn = space_cat.into())]
  SpaceCat,
  // #[assoc(to_spawn = sphericalcow.into())]
  #[assoc(to_spawn =
          (|pos|
           (name("spherical cow"),
            Interact::MultipleOptions(InteractMultipleOptions::DialogueTree(SPHERICAL_SPACE_COW)),
            SpaceObjectBundle::new(pos, 1.7, true, Visuals::sprite(MySprite::SphericalCow))))
          .into())]
  SphericalCow,
  #[assoc(to_spawn = zorp.into())]
  Zorp,
  #[assoc(to_spawn = floatingisland.into())]
  FloatingIsland,
  #[assoc(to_spawn = space_cop.into())]
  SpaceCop,
  #[assoc(to_spawn = space_wizard.into())]
  SpaceWizard,
  #[assoc(to_spawn = nomad.into())]
  Nomad,
  #[assoc(to_spawn = alien_soldier.into())]
  AlienSoldier,
  // #[assoc(to_spawn = gate.into())]
  // Gate,
  #[assoc(to_spawn = abandonedship.into())]
  AbandonedShip,
  // ...
  BlackHole,
  SpaceJellyfish,
  WormholePortal,
  SpaceWhale,
  AlienArtifact,
  SpaceStation,
  QuantumAnomaly,
  SpaceMine,
  NebulaCloud,
  SolarFlare,
  SpaceDebris,
  CometCluster,
  DysonSphere,
  AncientRuins,
  AlienOutpost,
  RepairDrone,
  SalvageYard,
  FuelDepot,
  SpaceCasino,
  ResearchLab,
  CloakedShip,
  SpaceBarnacle,
  CosmicSpore,
  TimeDistortion,
  GravityWell,
  IonStorm,
  PlasmaVortex,
  SpaceHermit,
  BountyHunter,
  SpacePiranhas,
  LivingCrystal,
  VoidEchoes,
  DimensionalRift,
  HolographicDecoy,
  SpaceGeyser,
  MagneticAnomaly,
  CrystallineEntity,
  QuantumComputer,
  NanoswarmCloud,
  TachyonField,
  PsiOrbNetwork,
  SpaceMirage,
  CosmicStringFragment,
  DarkMatterNode,
  AsteroidHatcher,
  SpaceLeviathan,
  VoidKraken,
  StarforgeRemnant,
  TemporalLoop
}
// struct Spawnable2(LazyCell<Box<dyn Fn(&mut Commands, Vec3)>>);

// impl Spawnable2 {
//   const fn from_fn<B: Bundle>(f: fn(Vec3) -> B) -> Self {
//     Self(LazyCell::new(|| {
//            Box::new(move |mut c, pos| {
//              let b = f(pos);
//              c.spawn(b);
//            })
//          }))
//   }
// }
// const SPACE_MAN: Spawnable2 = Spawnable2::from_fn(spaceman);
// const SPACE_MAN2: Spawnable2 = Spawnable2::from_fn(|| {
//   // OnceCell
//   |pos| {
//     loot_object(MySprite::SpaceMan,
//                 pos,
//                 1.3,
//                 "spaceman".to_string(),
//                 Item::Person)
//   }
// });

// type Lazy<T> = LazyCell<T>;
// fn lazy_from<A, B: Into<A>>(bf: impl FnOnce() -> B) -> LazyCell<A, _> {
//   LazyCell::new(|| bf().into())
// }
// const SPACEMAN2: LazyCell<ToSpawn> = LazyCell::new(|| {
//   from(|pos| {
//     loot_object(MySprite::SpaceMan,
//                 pos,
//                 1.3,
//                 "spaceman".to_string(),
//                 Item::Person)
//   })
// });
// struct Spawnable(Box<dyn FnOnce(&mut Commands) + Send + Sync>);
// impl Spawnable {
//   fn spawn(self, c: &mut Commands) { self.0(c); }
// }
// impl<B: Bundle> From<B> for Spawnable {
//   fn from(b: B) -> Self {
//     Self(Box::new(move |c| {
//            c.spawn(b);
//          }))
//   }
// }
// struct TranslationSpawnable(Box<dyn FnOnce(&mut Commands, Vec3)>);
// impl TranslationSpawnable {
//   fn spawn(self, c: &mut Commands, pos: Vec3) { self.0(c, pos); }
//   fn multi(coll: impl IntoIterator<Item = (Vec3, TranslationSpawnable)>) -> Self {
//     let coll = vec(coll);
//     Self(Box::new(move |mut c, pos| {
//            for (relpos, tospawn) in coll {
//              tospawn.spawn(&mut c, pos + relpos);
//            }
//          }))
//   }
// }

// #[derive(Clone)]
struct Spawnable(Box<dyn Fn(&mut Commands) + Send + Sync>);

impl Spawnable {
    fn spawn(&self, c: &mut Commands) { (self.0)(c); }
}

// impl<B: Bundle + Clone + 'static> From<B> for Spawnable {
//     fn from(b: B) -> Self {
//         Self(Box::new(move |c| {
//             c.spawn(b.clone());
//         }))
//     }
// }
// impl<B: Bundle> From<fn()->B> for Spawnable {
//     fn from(f: fn()->B) -> Self {
//         Self(Box::new(move |c| {
//             c.spawn(f());
//         }))
//     }
// }

// Implement From for functions that return a Bundle
impl<F, B> From<F> for Spawnable
where
    F: Fn() -> B + 'static + Send + Sync,
    B: Bundle + 'static,
{
    fn from(f: F) -> Self {
        Self(Box::new(move |c| {
            c.spawn(f());
        }))
    }
}

// #[derive(Clone)]
struct TranslationSpawnable(Box<dyn Fn(&mut Commands, Vec3) + Send + Sync>);

impl TranslationSpawnable {
    fn spawn(&self, c: &mut Commands, pos: Vec3) { (self.0)(c, pos); }
    fn multi(coll: impl IntoIterator<Item = (Vec3, TranslationSpawnable)>) -> Self {
        let coll: Vec<_> = coll.into_iter().collect();
        Self(Box::new(move |c, pos| {
            for (relpos, tospawn) in &coll {
                tospawn.spawn(c, pos + *relpos);
            }
        }))
    }
}

// impl<B: Bundle + Clone + 'static> From<B> for TranslationSpawnable {
//     fn from(b: B) -> Self {
//         Self(Box::new(move |c, pos| {
//             c.spawn(b.clone()).insert(Transform::from_translation(pos));
//         }))
//     }
// }

impl<F, B> From<F> for TranslationSpawnable
where
    F: Fn(Vec3) -> B + 'static + Send + Sync,
    B: Bundle + 'static,
{
    fn from(f: F) -> Self {
        Self(Box::new(move |c, pos| {
            c.spawn(f(pos));
        }))
    }
}
// impl<B: Bundle, F: FnOnce(Vec3) -> B + 'static> From<F> for TranslationSpawnable {
//   fn from(f: F) -> Self {
//     Self(Box::new(move |c, pos| {
//            c.spawn(f(pos));
//          }))
//   }
// }
impl SpawnableTemplate {
  const fn probs<const N: usize>(probs: &'static [(f32, SpawnableTemplate); N]) -> Self {
    Self::Probabilistic(probs.as_slice())
  }
  fn pick(self) -> Option<Self> {
    match self {
      Self::Probabilistic(manyprobs) => {
        for (p, v) in manyprobs.into_iter() {
          if prob(*p) {
            return v.pick();
          }
        }
        return None;
      }
      _ => Some(self)
    }
  }
}

pub fn from<B, A: From<B>>(b: B) -> A { A::from(b) }
const NORMAL_ASTEROID_FIELD: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.5, SpawnableTemplate::Asteroid),
                             (0.1, SpawnableTemplate::CrystalAsteroid),
                             (0.1, SpawnableTemplate::IceAsteroid),
                             (0.1, SpawnableTemplate::SpaceCoin),
                             (0.5, SpawnableTemplate::CrystalMonster),
                             (0.5, SpawnableTemplate::SphericalCow),
                             (0.5, SpawnableTemplate::Zorp),
                             (1.0, SpawnableTemplate::SpaceCat)]);
const VARIOUS_ASTEROIDS: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.5, SpawnableTemplate::Asteroid),
                             (0.4, SpawnableTemplate::CrystalAsteroid),
                             (1.0, SpawnableTemplate::IceAsteroid)]);
const ICE_ASTEROID_FIELD: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.6, SpawnableTemplate::IceAsteroid),
                             (0.1, SpawnableTemplate::Asteroid),
                             (0.1, SpawnableTemplate::CrystalAsteroid),
                             (1.0, SpawnableTemplate::AbandonedShip)]);

// fn ice_asteroid_field() -> Option<Spawnable> {
//   pick_from_probs([(0.6, Spawnable::IceAsteroid),
//                    (0.1, Spawnable::Asteroid),
//                    (0.1, Spawnable::CrystalAsteroid),
//                    (1.0, Spawnable::AbandonedShip)])
// }
fn pick_from_probs<T>(probs: impl IntoIterator<Item = (f32, fn() -> Option<T>)>)
                      -> Option<T> {
  for (p, f) in probs {
    if prob(p) {
      return f();
    }
  }
  return None;
}
// enum SpawnProbs{

//   const NON_COMBAT_ICE_ASTEROID_FIELD: Spawnable =
//     Spawnable::probs(&[(0.6, ICE_ASTEROID_FIELD),
//                        (0.5, NON_COMBAT_ZONE_THINGS),
//                        (1.0, NON_HOSTILE_NPCS)]);
//   const PIRATE_ICE_ASTEROID_FIELD: Spawnable = Spawnable::probs(&[(0.6, ICE_ASTEROID_FIELD),
//                                                                   (0.5, COMBAT_ZONE_THINGS),
//                                                                   (1.0,
//                                                                    SPACE_PIRATE_ZONE_THINGS)]);

//   const NON_HOSTILE_NPCS: Spawnable = Spawnable::probs(&[(0.3, Spawnable::NPC),
//                                                          (0.3, Spawnable::SpaceWizard),
//                                                          (0.4, Spawnable::Nomad),
//                                                          (1.0, Spawnable::SpaceCop)]);
//   const TRADING_ZONE: Spawnable = Spawnable::probs(&[(0.7, Spawnable::TradeStation),
//                                                      (1.0, Spawnable::AbandonedShip)]);
//   const COMBAT_ZONE_THINGS: Spawnable = Spawnable::probs(&[(0.5, Spawnable::HPBox),
//                                                            (0.2,
//                                                             Spawnable::TreasureContainer),
//                                                            (0.1, Spawnable::AbandonedShip),
//                                                            (0.3, Spawnable::SpaceMan),
//                                                            (1.0, Spawnable::SpaceCoin)]);
//   const NON_COMBAT_ZONE_THINGS: Spawnable =
//     Spawnable::probs(&[(0.1, Spawnable::SpaceCoin),
//                        (0.1, Spawnable::FloatingIsland),
//                        (0.1, Spawnable::SphericalCow),
//                        (0.1, Spawnable::SpaceCat),
//                        (1.0, Spawnable::SpaceCoin)]);

//   const SPACE_PIRATE_ZONE_THINGS: Spawnable =
//     Spawnable::probs(&[(0.6, Spawnable::SpacePirate),
//                        (0.1, Spawnable::TreasureContainer),
//                        (0.1, Spawnable::ThreeHostileTurrets),
//                        (0.1, Spawnable::HostileTurret),
//                        (1.0, Spawnable::SpacePirateBase)]);
//   const INVADERS: Spawnable = Spawnable::probs(&[(1.0, Spawnable::AlienSoldier)]);

//   const ANOMALY_ZONE: Spawnable = Spawnable::probs(&[(0.5, Spawnable::BlackHole),
//                                                      (0.3, Spawnable::WormholePortal),
//                                                      (1.0, Spawnable::QuantumAnomaly)]);

//   const EXOTIC_LIFE_ZONE: Spawnable = Spawnable::probs(&[(0.4, Spawnable::SpaceJellyfish),
//                                                          (0.3, Spawnable::SpaceWhale),
//                                                          (0.2, Spawnable::AlienArtifact),
//                                                          (1.0, Spawnable::SpaceCat)]);

//   // Probs spawn probability sets
//   const NEBULA_ZONE: Spawnable = Spawnable::probs(&[(1.0, Spawnable::NebulaCloud),
//                                                     (0.3, Spawnable::SolarFlare),
//                                                     (0.2, Spawnable::SpaceJellyfish),
//                                                     (1.0, Spawnable::IonStorm)]);

//   const SALVAGE_ZONE: Spawnable = Spawnable::probs(&[(0.7, Spawnable::SpaceDebris),
//                                                      (0.4, Spawnable::AbandonedShip),
//                                                      (0.3, Spawnable::SalvageYard),
//                                                      (1.0, Spawnable::RepairDrone)]);

//   const ANCIENT_ZONE: Spawnable = Spawnable::probs(&[(0.6, Spawnable::AncientRuins),
//                                                      (0.3, Spawnable::AlienArtifact),
//                                                      (0.2, Spawnable::DysonSphere),
//                                                      (1.0, Spawnable::StarforgeRemnant)]);

//   const FRONTIER_ZONE: Spawnable = Spawnable::probs(&[(0.5, Spawnable::AlienOutpost),
//                                                       (0.3, Spawnable::FuelDepot),
//                                                       (0.2, Spawnable::SpaceHermit),
//                                                       (1.0, Spawnable::BountyHunter)]);

//   const COSMIC_HAZARD_ZONE: Spawnable = Spawnable::probs(&[(0.4, Spawnable::GravityWell),
//                                                            (0.3, Spawnable::TimeDistortion),
//                                                            (0.2, Spawnable::VoidEchoes),
//                                                            (1.0, Spawnable::DimensionalRift)]);

//   const EXOTIC_ECOSYSTEM: Spawnable = Spawnable::probs(&[(0.4, Spawnable::SpaceBarnacle),
//                                                          (0.3, Spawnable::CosmicSpore),
//                                                          (0.2, Spawnable::SpacePiranhas),
//                                                          (1.0, Spawnable::LivingCrystal)]);

//   const HIGH_TECH_ZONE: Spawnable = Spawnable::probs(&[(0.4, Spawnable::QuantumComputer),
//                                                        (0.3, Spawnable::NanoswarmCloud),
//                                                        (0.2, Spawnable::HolographicDecoy),
//                                                        (1.0, Spawnable::PsiOrbNetwork)]);

//   const COSMIC_PHENOMENON: Spawnable = Spawnable::probs(&[(0.3, Spawnable::SpaceMirage),
//                                                           (0.3,
//                                                            Spawnable::CosmicStringFragment),
//                                                           (0.2, Spawnable::DarkMatterNode),
//                                                           (1.0, Spawnable::TachyonField)]);

//   const MEGAFAUNA_ZONE: Spawnable = Spawnable::probs(&[(0.4, Spawnable::AsteroidHatcher),
//                                                        (0.3, Spawnable::SpaceLeviathan),
//                                                        (0.2, Spawnable::VoidKraken),
//                                                        (1.0, Spawnable::CrystallineEntity)]);
//   const SPACE_PIRATE_ASTEROID_FIELD: Spawnable =
//     Spawnable::probs(&[(0.5, NORMAL_ASTEROID_FIELD),
//                        (0.2, COMBAT_ZONE_THINGS),
//                        (1.0, SPACE_PIRATE_ZONE_THINGS)]);

//   // ... (previous code remains unchanged)

//   const INVADER_ATTACK: Spawnable = Spawnable::probs(&[(0.4, NORMAL_ASTEROID_FIELD),
//                                                        (0.1, COMBAT_ZONE_THINGS),
//                                                        (0.3, NON_HOSTILE_NPCS),
//                                                        (0.2, Spawnable::WormHole),
//                                                        (0.1, Spawnable::SpaceStation),
//                                                        (1.0, INVADERS)]);
// }
const NON_COMBAT_ICE_ASTEROID_FIELD: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.6, ICE_ASTEROID_FIELD),
                             (0.5, NON_COMBAT_ZONE_THINGS),
                             (1.0, NON_HOSTILE_NPCS)]);
const PIRATE_ICE_ASTEROID_FIELD: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.6, ICE_ASTEROID_FIELD),
                             (0.5, COMBAT_ZONE_THINGS),
                             (1.0, SPACE_PIRATE_ZONE_THINGS)]);

const NON_HOSTILE_NPCS: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.3, SpawnableTemplate::NPC),
                             (0.3, SpawnableTemplate::SpaceWizard),
                             (0.4, SpawnableTemplate::Nomad),
                             (1.0, SpawnableTemplate::SpaceCop)]);
const TRADING_ZONE: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.7, SpawnableTemplate::TradeStation),
                             (1.0, SpawnableTemplate::AbandonedShip)]);
const COMBAT_ZONE_THINGS: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.5, SpawnableTemplate::HPBox),
                             (0.2, SpawnableTemplate::TreasureContainer),
                             (0.1, SpawnableTemplate::AbandonedShip),
                             (0.3, SpawnableTemplate::SpaceMan),
                             (1.0, SpawnableTemplate::SpaceCoin)]);
const NON_COMBAT_ZONE_THINGS: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.1, SpawnableTemplate::SpaceCoin),
                             (0.1, SpawnableTemplate::FloatingIsland),
                             (0.1, SpawnableTemplate::SphericalCow),
                             (0.1, SpawnableTemplate::SpaceCat),
                             (1.0, SpawnableTemplate::SpaceCoin)]);

const SPACE_PIRATE_ZONE_THINGS: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.6, SpawnableTemplate::SpacePirate),
                             (0.1, SpawnableTemplate::TreasureContainer),
                             (0.1, SpawnableTemplate::ThreeHostileTurrets),
                             (0.1, SpawnableTemplate::HostileTurret),
                             (1.0, SpawnableTemplate::SpacePirateBase)]);
const INVADERS: SpawnableTemplate =
  SpawnableTemplate::probs(&[(1.0, SpawnableTemplate::AlienSoldier)]);

const ANOMALY_ZONE: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.5, SpawnableTemplate::BlackHole),
                             (0.3, SpawnableTemplate::WormholePortal),
                             (1.0, SpawnableTemplate::QuantumAnomaly)]);

const EXOTIC_LIFE_ZONE: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.4, SpawnableTemplate::SpaceJellyfish),
                             (0.3, SpawnableTemplate::SpaceWhale),
                             (0.2, SpawnableTemplate::AlienArtifact),
                             (1.0, SpawnableTemplate::SpaceCat)]);

// Probs spawn probability sets
const NEBULA_ZONE: SpawnableTemplate =
  SpawnableTemplate::probs(&[(1.0, SpawnableTemplate::NebulaCloud),
                             (0.3, SpawnableTemplate::SolarFlare),
                             (0.2, SpawnableTemplate::SpaceJellyfish),
                             (1.0, SpawnableTemplate::IonStorm)]);

const SALVAGE_ZONE: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.7, SpawnableTemplate::SpaceDebris),
                             (0.4, SpawnableTemplate::AbandonedShip),
                             (0.3, SpawnableTemplate::SalvageYard),
                             (1.0, SpawnableTemplate::RepairDrone)]);

const ANCIENT_ZONE: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.6, SpawnableTemplate::AncientRuins),
                             (0.3, SpawnableTemplate::AlienArtifact),
                             (0.2, SpawnableTemplate::DysonSphere),
                             (1.0, SpawnableTemplate::StarforgeRemnant)]);

const FRONTIER_ZONE: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.5, SpawnableTemplate::AlienOutpost),
                             (0.3, SpawnableTemplate::FuelDepot),
                             (0.2, SpawnableTemplate::SpaceHermit),
                             (1.0, SpawnableTemplate::BountyHunter)]);

const COSMIC_HAZARD_ZONE: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.4, SpawnableTemplate::GravityWell),
                             (0.3, SpawnableTemplate::TimeDistortion),
                             (0.2, SpawnableTemplate::VoidEchoes),
                             (1.0, SpawnableTemplate::DimensionalRift)]);

const EXOTIC_ECOSYSTEM: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.4, SpawnableTemplate::SpaceBarnacle),
                             (0.3, SpawnableTemplate::CosmicSpore),
                             (0.2, SpawnableTemplate::SpacePiranhas),
                             (1.0, SpawnableTemplate::LivingCrystal)]);

const HIGH_TECH_ZONE: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.4, SpawnableTemplate::QuantumComputer),
                             (0.3, SpawnableTemplate::NanoswarmCloud),
                             (0.2, SpawnableTemplate::HolographicDecoy),
                             (1.0, SpawnableTemplate::PsiOrbNetwork)]);

const COSMIC_PHENOMENON: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.3, SpawnableTemplate::SpaceMirage),
                             (0.3, SpawnableTemplate::CosmicStringFragment),
                             (0.2, SpawnableTemplate::DarkMatterNode),
                             (1.0, SpawnableTemplate::TachyonField)]);

const MEGAFAUNA_ZONE: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.4, SpawnableTemplate::AsteroidHatcher),
                             (0.3, SpawnableTemplate::SpaceLeviathan),
                             (0.2, SpawnableTemplate::VoidKraken),
                             (1.0, SpawnableTemplate::CrystallineEntity)]);
const SPACE_PIRATE_ASTEROID_FIELD: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.5, NORMAL_ASTEROID_FIELD),
                             (0.2, COMBAT_ZONE_THINGS),
                             (1.0, SPACE_PIRATE_ZONE_THINGS)]);

// ... (previous code remains unchanged)

const INVADER_ATTACK: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.4, NORMAL_ASTEROID_FIELD),
                             (0.1, COMBAT_ZONE_THINGS),
                             (0.3, NON_HOSTILE_NPCS),
                             (0.2, SpawnableTemplate::WormHole),
                             (0.1, SpawnableTemplate::SpaceStation),
                             (1.0, INVADERS)]);

type ManyProbs = &'static [(f32, SpawnableProbsVariants)];
enum SpawnableProbsVariants {
  Guaranteed(SpawnableTemplate),
  ManyProbs(ManyProbs)
}
type SpawnableProbs = LazyCell<SpawnableProbsVariants>;

#[derive(Clone)]
enum SpawnableThing {
    Concrete(Spawnable),
    Probabilistic(&'static [(f32, SpawnableThing)]),
}
const fn concrete(spawnable:Spawnable)->SpawnableThing{
  SpawnableThing::Concrete(spawnable)
}
static INVADER_ATTACK2: SpawnableThing =
  SpawnableThing::Probabilistic(
    &[(0.4, NORMAL_ASTEROID_FIELD),
      (0.1, COMBAT_ZONE_THINGS),
      (0.3, NON_HOSTILE_NPCS),
      (0.2, concrete(Spawnable::WormHole)),
      (0.1, concrete(Spawnable::SpaceStation)),
      (1.0, INVADERS)]);
type SpawnableThing2 = fn()->&'static[(f32,Spawnable)];
static INVADER_ATTACK2: SpawnableThing2 = ||{
  &[(0.4, NORMAL_ASTEROID_FIELD()),
    (0.1, COMBAT_ZONE_THINGS()),
    (0.3, NON_HOSTILE_NPCS()),
    (0.2, Spawnable::WormHole),
    (0.1, Spawnable::SpaceStation),
    (1.0, INVADERS())]
}
// static INVADER_ATTACK2: SpawnableProbs = LazyCell::new(|| {
//   [(0.4, NORMAL_ASTEROID_FIELD),
//    (0.1, COMBAT_ZONE_THINGS),
//    (0.3, NON_HOSTILE_NPCS),
//    (0.2, Spawnable::WormHole.to_spawn()),
//    (0.1, Spawnable::SpaceStation.to_spawn()),
//    (1.0, INVADERS)]
// });
// fn invader_attack() -> Option<Spawnable> {
//   pickrandom[(0.4, NORMAL_ASTEROID_FIELD()),
//              (0.1, COMBAT_ZONE_THINGS()),
//              (0.3, NON_HOSTILE_NPCS()),
//              (0.2, Spawnable::WormHole),
//              (0.1, Spawnable::SpaceStation),
//              (1.0, INVADERS())]
// }
// const fn manyprobs(many: ManyProbs) -> SpawnableProbsVariants {
//   SpawnableProbsVariants::ManyProbs(many)
// }

// impl<const N: usize> From<fn() -> [(f32, SpawnableProbs); N]> for SpawnableProbs {
//   fn from(value: fn() -> [(f32, SpawnableProbs); N]) -> Self {
//     let l = LazyCell::new(value);
//     Self
//   }
// }
const TRADING_ZONE_PROBS: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.2, NORMAL_ASTEROID_FIELD),
                             (0.4, TRADING_ZONE),
                             (1.0, NON_HOSTILE_NPCS)]);

const ASTEROID_FIELD_PROBS: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.4, NORMAL_ASTEROID_FIELD),
                             (0.2, COMBAT_ZONE_THINGS),
                             (1.0, NON_HOSTILE_NPCS)]);

const SPACE_STATION_ZONE_PROBS: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.2, SpawnableTemplate::SpaceStation),
                             (0.5, VARIOUS_ASTEROIDS),
                             (0.5, NON_HOSTILE_NPCS),
                             (1.0, TRADING_ZONE)]);

const ANOMALY_CLUSTER_PROBS: SpawnableTemplate =
  SpawnableTemplate::probs(&[(1.0, ANOMALY_ZONE),
                             (0.2, COMBAT_ZONE_THINGS),
                             (1.0, EXOTIC_LIFE_ZONE)]);

const EXOTIC_LIFE_ZONE_PROBS: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.8, EXOTIC_LIFE_ZONE),
                             (0.3, NORMAL_ASTEROID_FIELD),
                             (0.1, ANOMALY_ZONE)]);

const MINEFIELD_ZONE_PROBS: SpawnableTemplate =
  SpawnableTemplate::probs(&[(0.7,
                              SpawnableTemplate::probs(&[(1.0,
                                                          SpawnableTemplate::SpaceMine)])),
                             (0.5, COMBAT_ZONE_THINGS),
                             (0.3, SPACE_PIRATE_ZONE_THINGS)]);

#[derive(Assoc, Copy, Clone, Hash, Eq, PartialEq, Debug)]
#[func(pub fn probs(&self) -> SpawnableTemplate)]
pub enum ZoneType {
  #[assoc(probs = SPACE_PIRATE_ASTEROID_FIELD)]
  SpacePirateAsteroidField,
  #[assoc(probs = INVADER_ATTACK)]
  InvaderAttack,
  #[assoc(probs = TRADING_ZONE_PROBS)]
  TradingZone,
  #[assoc(probs = ASTEROID_FIELD_PROBS)]
  AsteroidField,
  #[assoc(probs = NON_COMBAT_ICE_ASTEROID_FIELD)]
  IceAsteroidField,
  #[assoc(probs = PIRATE_ICE_ASTEROID_FIELD)]
  PirateIceAsteroidField,
  #[assoc(probs = SPACE_STATION_ZONE_PROBS)]
  SpaceStationZone,
  #[assoc(probs = ANOMALY_CLUSTER_PROBS)]
  AnomalyCluster,
  #[assoc(probs = EXOTIC_LIFE_ZONE_PROBS)]
  ExoticLifeZone,
  #[assoc(probs = MINEFIELD_ZONE_PROBS)]
  MinefieldZone
}
fn rangerand(lo: f32, hi: f32) -> f32 { lo.lerp(hi, rand::random::<f32>()) }
fn random_zone_name() -> String {
  String::from_utf8((0..4).map(|_| rangerand(0.0, 30.0) as u8).collect()).unwrap()
  // (0..4).map(|_| random::<char>()).collect()
}
#[derive(Component, Clone)]
struct GatesConnected(Entity, Entity);
#[derive(Component, Clone)]
struct Gate;
fn asteroid_scale() -> f32 { rangerand(0.8, 2.3) }
fn random_normalized_vector() -> Vec3 { random::<Quat>() * Vec3::X }
fn prob(p: f32) -> bool { p > rand::random::<f32>() }

#[derive(Component, Debug, Clone)]
pub struct Zone {
  pub faction_control: Option<Faction>,
  pub zone_radius: f32,
  pub zone_type: ZoneType,
  pub is_combat_zone: bool,
  //pub number_of_things: u32,
  pub planet_type: Option<PlanetType>
}

#[derive(Component, Clone)]
struct ZoneEntity {
  zone: Entity
}

impl Zone {
  fn spawn(&self,
           mut c: &mut Commands,
           zone_pos: Vec3 // , zone_entity: Entity
  ) {
    let num_objects = 60;
    c.spawn(sign(zone_pos, prettyfmt(self)));
    for _ in 0..num_objects {
      let object_pos =
        zone_pos + (random_normalized_vector() * self.zone_radius * rangerand(0.5, 1.0));
      if let Some(spawnable) = self.zone_type.probs().pick() {
        if let Some(to_spawn) = spawnable.to_spawn() {
          to_spawn.spawn(c, object_pos);
          // to_spawn.0(&mut c, object_pos);
        }
      }
    }
    if let Some(planet_type) = self.planet_type {
      let planet_distance = 700.0;
      let rel_pos = random_normalized_vector() * planet_distance;
      let sprite = match planet_type {
        PlanetType::MarsLikePlanet => MySprite::MarsLikePlanet,
        PlanetType::HabitablePlanet => MySprite::HabitablePlanet,
        PlanetType::IcePlanet => MySprite::IcePlanet,
        PlanetType::LavaPlanet => MySprite::LavaPlanet,
        PlanetType::SandPlanet => MySprite::SandPlanet,
        PlanetType::BrownGasGiant => MySprite::BrownGasGiant
      };
      c.spawn((Planet { planet_type,
                        population: (rangerand(30000.0, 300000.0) as u32).pow(1) },
               SpaceObjectBundle::new(zone_pos + rel_pos,
                                      100.0,
                                      false,
                                      Visuals::sprite(sprite))));
    }
  }
}

const MAX_ZONE_RANGE: f32 = 200.0;

#[derive(Component, Debug, PartialEq, Eq, Clone)]
struct InZone {
  in_player_zone: bool // zone: Option<Entity>
}

fn update_in_zone(player_query: Query<&Transform, With<Player>>,
                  mut entity_query: Query<(Entity, &Transform, Option<&mut InZone>)>,
                  zone_query: Query<(Entity, &Zone, &Transform)>,
                  mut c: Commands) {
  if let Ok(player_transform) = player_query.get_single() {
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

pub fn setup(playerq: Query<&Transform, With<Player>>,
             serv: Res<AssetServer>,
             mut meshes: ResMut<Assets<Mesh>>,
             mut materials: ResMut<Assets<StandardMaterial>>,

             mut c: Commands) {
  let sun_pos = Vec3::ZERO;
  let sun_scale = 300.0;
  let zone_dist_from_sun = 2000.0;
  let num_zones = 8;
  c.spawn(player(Vec3::Y * 1000.0));
  c.spawn((SpaceObjectBundle::new(sun_pos,
                                  sun_scale,
                                  false,
                                  Visuals::material_sphere(MyMaterial::GlowyMaterial2)),
           CubemapVisibleEntities::default(),
           CubemapFrusta::default(),
           PointLight { intensity: 3_000_000.0,
                        radius: 1.0,
                        range: 10000.0,
                        shadows_enabled: true,
                        color: Color::srgb(0.9, 0.8, 0.6),
                        ..default() }));
  for _ in 0..num_zones {
    let planet_type = if prob(0.6) {
      Some(pick([PlanetType::SandPlanet,
                 PlanetType::BrownGasGiant,
                 PlanetType::MarsLikePlanet,
                 PlanetType::LavaPlanet,
                 PlanetType::IcePlanet,
                 PlanetType::HabitablePlanet]).unwrap())
    } else {
      None
    };
    let zone_pos = sun_pos + (random_normalized_vector() * zone_dist_from_sun);

    let zone_type = pick([ZoneType::SpacePirateAsteroidField,
                          ZoneType::InvaderAttack,
                          ZoneType::PirateIceAsteroidField,
                          ZoneType::TradingZone,
                          ZoneType::AsteroidField,
                          ZoneType::IceAsteroidField,
                          ZoneType::SpaceStationZone]).unwrap();
    // let zone_entity = c.spawn_empty().id();
    let zone = Zone { zone_radius: 60.0,
                      is_combat_zone: false,
                      faction_control: None,
                      zone_type,
                      planet_type };
    zone.spawn(&mut c, zone_pos);
    c.spawn((zone, Transform::from_translation(zone_pos)));
  }

  c.spawn(PbrBundle {
    mesh: meshes.add(Circle::new(4.0)),
    material: materials.add(Color::WHITE),
    transform: Transform::from_rotation(Quat::from_rotation_x(-std::f32::consts::FRAC_PI_2)),
    ..default()
  });
  // c.spawn(SpaceObjectBundle::new( Visuals::MaterialSphere()) PbrBundle { mesh: serv.handle(&SPHERE),
  //                     material: materials.add(Color::srgb_u8(124, 144, 255)),
  //                     transform: Transform::from_xyz(0.0, 0.5, 0.0),
  //                     ..default() });

  let colorful_mat = serv.add(StandardMaterial::from(serv.add(colorful_texture())));
  // let randmat = || {
  //   serv.add(StandardMaterial::from(serv.handle(pick([&MUSHROOM_MAN,
  //                                                     &TREE,
  //                                                     &WATER,
  //                                                     &SNOW,
  //                                                     &HABITABLE_PLANET]).unwrap())))
  // };
  // light
  c.spawn(PointLightBundle { point_light: PointLight { shadows_enabled: true,
                                                       ..default() },
                             transform: Transform::from_xyz(4.0, 8.0, 4.0),
                             ..default() });

  let fov = std::f32::consts::PI / 4.0;

  let pitch_limit_radians = 1.0;
  let camera =
    (IsDefaultUiCamera,
     BLOOM_SETTINGS,
     // Skybox { image: skybox_handle.clone(),
     //          brightness: 600.0 },
     Camera2d,
     Camera3dBundle { camera: Camera { hdr: true,

                                       ..default() },
                      projection:
                        Projection::Perspective(PerspectiveProjection { fov, ..default() }),
                      exposure: bevy::render::camera::Exposure { ev100: 10.0 },
                      // tonemapping:
                      //   bevy::core_pipeline::tonemapping::Tonemapping::Reinhard,
                      ..default() },
     PanOrbitCamera { // radius: Some(5.0),

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
                      zoom_lower_limit: Some(5.0),
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
                      ..default() });
  c.spawn(camera);
  println("setup");
}

fn spawn_skybox(serv: Res<AssetServer>,
                mut images: ResMut<Assets<Image>>,
                mut camq: Query<Entity, With<Camera>>,
                mut c: Commands,
                mut skybox_handle: Local<Option<Handle<Image>>>,
                mut done: Local<bool>) {
  if let Ok(cam_entity) = camq.get_single()
     && !*done
  {
    let skybox_handle = skybox_handle.get_or_insert_with(|| {
                                       serv.load(format!("embedded://{}",
                                                         MySprite::NasaStarmap.path()))
                                     })
                                     .clone();
    println("hmm1");
    if let Some(mut skybox) = images.get_mut(&skybox_handle) {
      println("hmm2");
      skybox.reinterpret_stacked_2d_as_array(skybox.height() / skybox.width());

      skybox.texture_view_descriptor =
        Some(TextureViewDescriptor { dimension: Some(bevy::render::render_resource::TextureViewDimension::Cube),
                                     ..default() });
      c.entity(cam_entity)
       .insert(Skybox { image: skybox_handle.clone(),
                        brightness: 600.0 });
      *done = true;
    }
  }
}
#[bevy_main]
pub fn main() {
  let gravity = avian3d::dynamics::integrator::Gravity::ZERO;
  let solver_config = SolverConfig { contact_damping_ratio: 0.5,
                                     // contact_frequency_factor: 1.5,
                                     // max_overlap_solve_speed: 4.0,
                                     // warm_start_coefficient: 1.0,
                                     // restitution_threshold: 1.0,
                                     // restitution_iterations: 1,
                                     ..default() };
  let address_mode = ImageAddressMode::ClampToBorder;
  let default_sampler = ImageSamplerDescriptor { // address_mode_u: address_mode,
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
                                                 ..default() };
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
        .set(ImagePlugin{default_sampler})
        .set(WindowPlugin {
          primary_window: Some(Window {
            // resolution: WindowResolution


            mode:WindowMode::Windowed,

            present_mode: bevy::window::PresentMode::AutoVsync,
            title: "bevy space game".to_string(),
            canvas: Some("#bevy".to_string()),
            ..default()
          }),
          ..default()
        }),
      bevy_vox_scene::VoxScenePlugin,
      bevy_sprite3d::Sprite3dPlugin,
      bevy_panorbit_camera::PanOrbitCameraPlugin,
      bevy_mod_billboard::prelude::BillboardPlugin,
      bevy_mod_picking::DefaultPickingPlugins,
      avian3d::PhysicsPlugins::default(),

      // SickleUiPlugin,
      // HierarchyTreeViewPlugin,

      QuillPlugin,
      QuillOverlaysPlugin,
    ))// .add_plugins(add_global_highlight)
    .add_event::<GuiInputEvent>()
    .init_resource::<UIData>()
    .init_resource::<TimeTicks>()
    .insert_resource(gravity)
    .insert_resource(solver_config)
  // .insert_resource(ClearColor(Color::BLACK))
    .insert_resource(bevy_mod_picking::debug::DebugPickingMode::Normal)
    .init_asset::<bevy_vox_scene::VoxelScene>()
    .insert_resource(AMBIENT_LIGHT)
    .add_systems(Startup, (// preload_assets,
                           setup// ,add_global_highlight
        ,ui).chain())

  // .add_systems(Startup, setup.run_if(in_state))
    .add_systems(Update,(
      close_on_esc,
      spawn_mushroom_man,
      player_movement,
      camera_follow_player,
      increment_time,
      origin_time,
      timed_animation_system,
      combat_visual_effects,
      // missile_movement,
      // laser_system,
      // explosion_system,
      player_target_interaction,
    ).chain())
    .add_systems(Update,(
      update_in_zone,
      combat_system,
      // spawn_missile,
      warp,
      ui,
      spawn_skybox,
      npc_movement,
      interact,
      navigation,
      click_target,
      // player_target_visuals,
      set_visuals,
      visuals
    ).chain())
    .run();
}

// trunk build --release --public-url "bevyspacegame" --filehash false

// trunk serve

// cargo check --target wasm32-unknown-unknown
// cargo run --target x86_64-unknown-linux-gnu
// cargo check --target x86_64-unknown-linux-gnu

comment! {

// #[derive(Clone)]
// enum SpawnableNPCKind {
//   NPC,
//   MushroomMan,
//   Enemy
// }
// #[derive(Debug, Assoc, Clone)]
// #[func(pub fn scale(&self) -> f32)]
// #[func(pub fn sprite(&self) -> MySprite)]
// #[func(pub fn item(&self) -> Item)]
// enum LootObjectKind {
//   #[assoc(scale = asteroid_scale())]
//   #[assoc(sprite = MySprite::CrystalAsteroid)]
//   #[assoc(item = Item::Crystal)]
//   CrystalAsteroid,
//   #[assoc(scale = asteroid_scale())]
//   #[assoc(sprite = MySprite::IceAsteroid)]
//   #[assoc(item = Item::DiHydrogenMonoxide)]
//   IceAsteroid,
//   #[assoc(scale = 1.3)]
//   #[assoc(sprite = MySprite::SpaceCat)]
//   #[assoc(item = Item::SpaceCat)]
//   SpaceCat,
//   #[assoc(scale = 1.2)]
//   #[assoc(sprite = MySprite::Coin)]
//   #[assoc(item = Item::Money)]
//   Money
// }
// A static trait object using Box with 'static lifetime
// static INSTANCE: &'static dyn MyTrait = &MyStruct;

// fn main() { INSTANCE.do_something(); }

// struct EntityTemplate(Box<dyn Fn(&mut Commands, Vec3)>);
// struct EntityTemplate(&'static dyn Fn(&mut Commands, Vec3));

// impl EntityTemplate {
//   const fn new<B: Bundle>(func: fn(Vec3) -> B) -> Self {
//     let j = move |c: &mut Commands<'_, '_>, v: Vec3| {
//       let b = func(v);
//       c.spawn(b);
//     };
//     Self(&j)
//   }
// }
// static SPACE_CAT: EntityTemplate = EntityTemplate::new(|pos| {
//   loot_object(MySprite::SpaceCat,
//               pos,
//               1.3,
//               "space cat".to_string(),
//               Item::SpaceCat)
// });
// use std::{cell::{LazyCell, OnceCell},
//           rc::Weak,
//           sync::{Arc, LazyLock, OnceLock}};
// const fn create_fn_pointer(func: fn(i32) -> i32) -> fn(i32) -> i32 {
//   fn wrapper(input: i32) -> i32 {
//     func(input) // Call the input function directly
//   }
//   wrapper // Return the wrapper function
// }
// trait EntityTemplateInner: Send + Sync {
//   fn spawn(&self, c: &mut Commands, v: Vec3);
// }
// impl<F: Fn(&mut Commands, Vec3) + Send + Sync> EntityTemplateInner for F {
//   fn spawn(&self, c: &mut Commands, v: Vec3) { self(c, v); }
// }
// struct EntityTemplate(&'static dyn EntityTemplateInner);

// impl EntityTemplate {
//   const fn new<B: Bundle>(func: impl Fn(Vec3) -> B + Send + Sync) -> Self {
//     // &'static dyn EntityTemplateInner
//     let f: dyn EntityTemplateInner = {
//       move |c: &mut Commands, v| {
//         c.spawn(func(v));
//       }
//     };

//     Self(&f)
//     // let f = move |c: &mut Commands, v: Vec3| {
//     //   let b = func(v);
//     //   c.spawn(b);
//     // };
//     // let bf = Box::new(f);
//     // Self(OnceLock::new())
//   }
// }

// static STATIC_BOX: Box<u32> = Box::new(5);
// #[derive(Assoc, Copy, Clone, Hash, Eq, PartialEq)]
// #[func(pub fn spawn(&self,mut c:&mut Commands) -> EntityCommands)]
// enum EntityTemplate {
//   #[assoc(spawn = c.spawn(loot_object(MySprite::SpaceCat,default(),1.3,"space cat".into(),Item::SpaceCat)))]
//   SpaceCat,
//   NeutralNPC,
//   MushroomMan,
//   Enemy,
//   Asteroid,
//   SphericalCow,
//   CrystalAsteroid,
//   IceAsteroid,
//   Money
// }

// fn k() { Weak }
#[derive(Clone)]
enum InertSpaceThing {
  // #[assoc(scale = "white_corners.png")]
  Asteroid,
  // #[assoc(path = "white_corners.png")]
  SphericalCow
}
#[derive(Clone)]
enum SpawnableSpaceObjectKind {
  SpaceObject {
    scale: f32,
    can_move: bool,
    visuals: Visuals
  },
  // NPC {
  //   kind: SpawnableNPCKind
  // },
  LootObject {
    scale: f32,
    sprite: MySprite,
    item: Item,
    name: String
  },
  SpaceCat,
  NeutralNPC,
  MushroomMan,
  Enemy,
  OldLootObject {
    kind: LootObjectKind
  },
  // Item {
  //   item: Item
  // },
  InertSpaceThing {
    kind: InertSpaceThing
  },
  Planet {
    planet: Planet
  }
}
// #[derive(Clone)]
struct SpawnableSpaceObject<'t> {
  c: &'t mut Commands<'t, 't>,
  translation: Vec3,
  kind: SpawnableSpaceObjectKind
}
  // fn spawn_at(&self, mut c: &mut Commands, pos: Vec3) { self.spawn() }

// impl SpawnableSpaceObject {
//   fn with_kind(self, kind: SpawnableSpaceObjectKind) -> Self { Self { kind, ..self } }
//   fn spawn<'t>(self, mut c: &'t mut Commands<'_, '_>) -> &'t mut EntityCommands<'t> {
//     type Kind = SpawnableSpaceObjectKind;
//     let Self { translation,
//                kind,
//                mut c } = self.clone();
//     match kind {
//       Kind::SpaceObject { scale,
//                           can_move,
//                           visuals } => {
//         &mut c.spawn(SpaceObjectBundle::new(translation, scale, can_move, visuals))
//       }
//       Kind::OldLootObject { kind } => {
//         let scale = kind.scale();
//         let sprite = kind.sprite();
//         let item = kind.item();
//         &mut c.spawn(item_in_space(sprite, translation, scale, debugfmt(kind), item))
//       }
//       Kind::InertSpaceThing { kind } => todo!(),
//       Kind::Planet { planet } => todo!(),
//       Kind::LootObject { scale,
//                          sprite,
//                          item,
//                          name } => {
//         self.with_kind(Kind::SpaceObject { scale,
//                                            can_move: true,
//                                            visuals: Visuals::sprite(sprite) })
//             .spawn(c)
//             .insert(Name::new(name))
//             .insert(Interact::Item(item))
//         // .reborrow()
//       }
//       Kind::SpaceCat => self.with_kind(Kind::LootObject { scale: 1.3,
//                                                           sprite: MySprite::SpaceCat,
//                                                           item: Item::SpaceCat,
//                                                           name: "space cat".into() })
//                             .spawn(c),
//       Kind::MushroomMan => todo!(),
//       // Kind::NPC => todo!(),
//       Kind::NeutralNPC => todo!(),
//       Kind::Enemy => todo!(),
//       Kind::InertSpaceThing { kind } => todo!()
//     }
//   }
// }
// fn preload_assets(mut c: Commands, serv: Res<AssetServer>) {
//   let mut preload_mesh = |genmesh: GenMesh| {
//     let handle = serv.add(genmesh.gen());
//     MESH_HANDLES.get(genmesh as usize)
//                 .unwrap()
//                 .get_or_init(|| handle.clone());
//     c.spawn(handle);
//   };
//   preload_mesh(GenMesh::SPHERE);
//   preload_mesh(GenMesh::BILLBOARD_MESH_SQUARE);
//   let mut preload_image = |image: MySprite| {
//     let handle = serv.load(image.path());
//     SPRITE_HANDLES.get(image as usize)
//                   .unwrap()
//                   .get_or_init(|| handle.clone());
//     c.spawn(handle);
//   };
//   preload_image(MySprite::NASA_STARMAP);
//   let mut preload_material = |material: MyMaterial| {
//     let handle = serv.add(material.val());
//     MATERIAL_HANDLES.get(material as usize)
//                     .unwrap()
//                     .get_or_init(|| handle.clone());
//     c.spawn(handle);
//   };
//   preload_material(MyMaterial::HOVERED_MATERIAL);
//   preload_material(MyMaterial::PRESSED_MATERIAL);
//   preload_material(MyMaterial::SELECTED_MATERIAL);
//   preload_material(MyMaterial::INVISIBLE_MATERIAL);
//   println("got here");
// }
// fn add_global_highlight(mut global_highlight: ResMut<GlobalHighlight<StandardMaterial>>,
//                         serv: Res<AssetServer>) {
  pub fn approach_target(mut approachers_q: Query<(&ApproachTarget,
                                                   &Transform,
                                                   &mut ExternalForce)>,
                         targets_q: Query<&Transform>) {
    // TimestepMode
    for (&approacher_target, &approacher_transform, mut approacher_force) in &mut approachers_q
    {
      let target_translation_and_range: Option<(Vec3, f32)> = match approacher_target {
        ApproachTarget::None => None,
        ApproachTarget::Translation { translation, range } => Some((translation, range)),
        ApproachTarget::Entity { entity, range } => {
          targets_q.get(entity)
                   .ok()
                   .map(|&Transform { translation, .. }| (translation, range))
        }
      };
      if let Some((target_translation, range)) = target_translation_and_range {
        let max_force = 1.0;
        let diff = target_translation - approacher_transform.translation;
        let dist = diff.length();
        if dist > range && dist > 0.1 {
          approacher_force.apply_force(diff.normalize() * max_force);
        }
      }
    }
  }

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
  pub fn integration_parameters() -> IntegrationParameters {
    IntegrationParameters { damping_ratio: 4.0,
                            // dt: todo!(),
                            // min_ccd_dt: todo!(),
                            // erp: todo!(),
                            // joint_erp: todo!(),
                            // joint_damping_ratio: todo!(),
                            // warmstart_coefficient: todo!(),
                            // length_unit: todo!(),
                            // normalized_allowed_linear_error: todo!(),
                            // normalized_max_penetration_correction: todo!(),
                            // normalized_prediction_distance: todo!(),
                            // num_solver_iterations: todo!(),
                            // num_additional_friction_iterations: todo!(),
                            // num_internal_pgs_iterations: todo!(),
                            // num_internal_stabilization_iterations: todo!(),
                            // min_island_size: todo!(),
                            // max_ccd_substeps: todo!(),
                            ..default() }
  }


}
