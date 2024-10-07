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

pub use bevy::prelude::Name;
use {avian3d::prelude::*,
     bevy::{app::AppExit,
            asset::{AssetServer, Handle},
            core_pipeline::{bloom::{BloomCompositeMode, BloomPrefilterSettings,
                                    BloomSettings},
                            Skybox},
            ecs::entity::EntityHashMap,
            math::{primitives, Vec3},
            pbr::{CubemapVisibleEntities, StandardMaterial},
            prelude::*,
            render::{primitives::CubemapFrusta,
                     render_resource::TextureViewDescriptor,
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
     rust_utils::{comment, debug_println, debugfmt, filter_map, map, mapv, prettyfmt, println, sort_by_key, sum, take, vec,
                  MutateTrait},
     ui::{ui_root_thing_in_the_world, Message, UIData, UIMainView}};

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
  #[assoc(path = "wormhole.png")]
  WormHole,
  #[assoc(path = "gate.png")]
  Gate,
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
// #[derive(Resource, Default)]
// pub struct Handles {
//   pub sprites: HashMap<MySprite, Handle<Image>>,
//   pub meshes: HashMap<GenMesh, Handle<Mesh>>,
//   pub materials: HashMap<MyMaterial, Handle<StandardMaterial>>,
//   pub scenes: HashMap<MyScene, Handle<Scene>>
// }
#[derive(Component, Clone, PartialEq, Eq, Default)]
pub struct Visuals {
  text: Option<String>,
  material_mesh: Option<(MyMaterial, GenMesh)>,
  sprite: Option<MySprite>,
  targeted: bool
}

impl Visuals {
  fn none() -> Self { default() }
  fn sprite(sprite: MySprite) -> Self {
    Self { sprite: Some(sprite),
           ..default() }
  }
  fn material_sphere(material: MyMaterial) -> Self {
    Self { material_mesh: Some((material, GenMesh::Sphere)),
           ..default() }
  }
  fn with_text(self, text: impl ToString) -> Self {
    Self { text: Some(text.to_string()),
           ..self }
  }
}
pub fn set_visuals(mut visuals_q: Query<(Entity, &mut Visuals)>,
                   mut player_q: Query<&Player>) {
  if let Ok(player) = player_q.get_single() {
    for (e, mut visuals) in &mut visuals_q {
      let should_have_target = player.target() == Some(e);
      let has_target = visuals.as_ref().targeted;
      if has_target != should_have_target {
        visuals.targeted = should_have_target;
      }
    }
  }
}
#[derive(Component)]
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
#[derive(Component, Clone, Debug)]
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

pub fn spawn_missile(playerq: Query<(&Transform, &Player)>,
                     // targetq: Query<(Entity, &GlobalTransform)>,
                     // time: Res<TimeTicks>,
                     keyboard_input: Res<ButtonInput<KeyCode>>,
                     mut c: Commands) {
  if let Ok((player_transform, player)) = playerq.get_single()
     && keyboard_input.just_pressed(KeyCode::KeyF)
     && let Some(target) = player.target()
  {
    c.spawn(missile(player_transform.translation, target));
  }
}

pub const EXPLOSION_TIME_TICKS: u32 = 50;
#[derive(Component, Default)]
pub struct Explosion {
  pub init_time: u32
}
pub fn explosion(pos: Vec3, scale: f32, init_time: u32) -> impl Bundle {
  (
    Explosion{init_time},
    Visuals::material_sphere(MyMaterial::ExplosionMaterial),
    SpatialBundle::from_transform(Transform::from_scale(Vec3::splat(scale))
                                  .with_translation(pos)),
  )
}
pub fn explosion_system(mut explosionq: Query<(Entity, &mut Explosion, &mut Transform)>,
                        time: Res<TimeTicks>,
                        mut c: Commands) {
  for (explosion_entity, explosion, mut transform) in &mut explosionq {
    let explosion_age = time.0 - explosion.init_time;
    let frac = (time.0 as f32 - explosion.init_time as f32) / EXPLOSION_TIME_TICKS as f32;
    transform.scale *= 1.03;
    if explosion_age > EXPLOSION_TIME_TICKS {
      // println(explosion_age);
      c.entity(explosion_entity).despawn_recursive();
    }
  }
}
#[derive(Component, Default)]
pub struct Combat {
  pub hp: u32
}
impl Combat {
  fn damage(&mut self, n: u32) { self.hp = self.hp.saturating_sub(n); }
  fn hp_depleted(&self) -> bool { self.hp == 0 }
}
#[derive(Component)]
pub struct IsHostile(pub bool);
pub fn combat(mut missileq: Query<(Entity, &mut Transform, &Missile)>,
              mut targetq: Query<(&GlobalTransform, Option<&mut Combat>)>,
              time: Res<TimeTicks>,
              keyboard_input: Res<ButtonInput<KeyCode>>,
              mut c: Commands) {
  let missile_damage = 10;
  let missile_speed = 6.0;
  let missile_hit_range = missile_speed * 1.2;
  let missile_chase_condition = |target| true;
  for (missile_entity, mut missile_transform, &Missile { target }) in &mut missileq {
    if let Ok((target_globaltransform, mut ocombat)) = targetq.get_mut(target)
       && missile_chase_condition(target)
    {
      let target_pos = target_globaltransform.translation();
      let rel = target_pos - missile_transform.translation;
      if rel.length() < missile_hit_range {
        // println("missile hit");
        c.entity(missile_entity).despawn_recursive();
        if let Some(mut combat) = ocombat.as_mut() {
          combat.damage(missile_damage);
          if combat.hp_depleted() {
            c.entity(target).despawn_recursive();
            c.spawn(explosion(target_pos, 2.0, time.0));
          }
        }
      } else {
        missile_transform.translation += rel.normalize_or_zero() * missile_speed;
      }
    } else {
      c.entity(missile_entity).despawn_recursive();
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
fn enemy_shoot_player(mut playerq: Query<(Entity, &Transform), With<Player>>,
                      mut hostileq: Query<(Entity, &IsHostile, &Transform)>,
                      mut c: Commands,
                      time: Res<TimeTicks>,
                      targetq: Query<(&Transform,)>) {
  if let Ok((player, player_transform)) = playerq.get_single() {
    let player_pos = player_transform.translation;
    let shoot_time_between = 60;
    let can_see_player = |enemy_entity| {
      hostileq.get(enemy_entity)
              .map_or(false, |(_, _, Transform { translation, .. })| {
                translation.distance(player_pos) < ENEMY_SEE_PLAYER_RANGE
              })
    };
    for (enemy, ishostile, enemy_transform) in &hostileq {
      if (time.0 % shoot_time_between == 0) && ishostile.0 && can_see_player(enemy) {
        c.spawn(missile(enemy_transform.translation, player));
      }
    }
  }
}
fn player_target_interaction(keys: Res<ButtonInput<KeyCode>>,
                             mut playerq: Query<(&mut Player, &Transform)>,
                             mut hostileq: Query<(Entity, &IsHostile, &Transform)>,
                             mut c: Commands,
                             time: Res<TimeTicks>,
                             targetq: Query<(&Transform,)>) {
  let shoot_time_between = 60;
  let can_see_target = |e| true;
  if let Ok((mut player, player_transform)) = playerq.get_single_mut() {
    let player_pos = player_transform.translation;

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
        state.shooting.update(std::ops::Not::not);
      }
      if keys.just_pressed(KeyCode::KeyQ) {
        state.approaching.update(std::ops::Not::not);
      }
      if keys.just_pressed(KeyCode::KeyF) {
        c.spawn(missile(player_pos, state.target));
      }
      if state.shooting && (time.0 % shoot_time_between == 0) {
        c.spawn(missile(player_pos, state.target));
      }
      if keys.just_pressed(KeyCode::KeyX) {
        player.untarget();
      }
    } else {
      player.untarget();
    }
  }
}
#[derive(Component, Default)]
pub struct Container(pub HashSet<Entity>);
impl Container {
  pub fn empty() -> Container { Container::default() }
}
pub fn name(s: &'static str) -> Name { Name::new(s) }
#[derive(Component)]
pub struct TimedAnimation {
  pub num_frames: usize,
  pub time_per_frame_in_ticks: usize
}
#[derive(Component)]
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
#[derive(Bundle)]
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
const PLAYER_FORCE: f32 = 130.0;
const PLAYER_SCALE: f32 = 1.2;
fn player(translation: Vec3) -> impl Bundle {
  (Player::default(),
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
enum Faction {
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

#[derive(Component)]
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

#[derive(Component)]
pub struct Enemy;

struct NPCTemplate {
  scale: f32,
  name: &'static str,
  thrust: f32,
  faction: Faction,
  hp: u32,
  sprite: MySprite
}

// pub const SPACE_PIRATE: NPCTemplate = NPCTemplate { scale: NORMAL_NPC_SCALE,
//                                                     name: "Space Pirate",
//                                                     thrust: 140.0,
//                                                     faction: Faction::SpacePirates,
//                                                     hp: 50,
//                                                     sprite: MySprite::SpaceshipRed };
// impl Default for NPCTemplate {
//   fn default() -> Self {
//     Self { scale: NORMAL_NPC_SCALE,
//            name: "npc",
//            thrust: 200.0,
//            faction: default(),
//            hp: 40,
//            sprite: MySprite::SpaceshipGreen }
//   }
// }
// impl NPCTemplate {
//   fn bundle(self) -> impl Bundle {
//     let Self { scale,
//                name,
//                thrust,
//                faction,
//                hp,
//                sprite } = self;
//     (Name::new(name),
//      Navigation::new(thrust),
//      NPC { follow_target: None,
//            faction },
//      Combat { hp },
//      SpaceObjectBundle::new(pos, scale, true, Visuals::sprite(sprite)))
//   }
// }
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
   Combat { hp },
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
pub fn space_pirate(pos: Vec3) -> impl Bundle {
  (IsHostile(true),
   scaled_npc(pos,
              NORMAL_NPC_SCALE,
              "space pirate",
              NORMAL_NPC_THRUST,
              Faction::SpacePirates,
              50,
              MySprite::SpaceshipRed))
}
pub fn space_pirate_base(pos: Vec3) -> impl Bundle {
  (Combat { hp: 120 },
   Interact::Describe,
   name("space pirate base"),
   SpaceObjectBundle::new(pos, 4.0, false, Visuals::sprite(MySprite::SpacePirateBase)))
}
pub fn space_station(pos: Vec3) -> impl Bundle {
  (Combat { hp: 120 },
   Interact::Describe,
   name("space station"),
   SpaceObjectBundle::new(pos, 4.0, false, Visuals::sprite(MySprite::SpaceStation)))
}
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
   scaled_npc(pos,
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
   scaled_npc(pos,
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
  (Interact::Describe,
   SpaceObjectBundle::new(pos,
                          1.5,
                          false,
                          Visuals::sprite(MySprite::Sign).with_text(text)))
}
pub fn wormhole(pos: Vec3) -> impl Bundle {
  (Interact::Describe,
   name("wormhole"),
   SpaceObjectBundle::new(pos, 4.0, false, Visuals::sprite(MySprite::WormHole)))
}
pub fn asteroid(pos: Vec3) -> impl Bundle {
  (Interact::Asteroid,
   CanBeFollowedByNPC,
   SpaceObjectBundle::new(pos,
                          asteroid_scale(),
                          false,
                          Visuals::sprite(MySprite::Asteroid)))
}

fn loot_object(image: MySprite,
               pos: Vec3,
               scale: f32,
               name: impl ToString,
               item_type: Item)
               -> impl Bundle {
  (Name::new(name.to_string()),
   Interact::Item(item_type),
   SpaceObjectBundle::new(pos, scale, true, Visuals::sprite(image)))
}
fn space_cat(pos: Vec3) -> impl Bundle {
  loot_object(MySprite::SpaceCat,
              pos,
              1.3,
              "space cat".to_string(),
              Item::SpaceCat)
}
fn space_man(pos: Vec3) -> impl Bundle {
  loot_object(MySprite::SpaceMan,
              pos,
              1.3,
              "spaceman".to_string(),
              Item::Person)
}
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
   Interact::Container(vec(contents)),
   SpaceObjectBundle::new(translation, 2.1, true, Visuals::sprite(MySprite::Container)))
}
pub fn hp_box(pos: Vec3) -> impl Bundle {
  (name("hp box"), SpaceObjectBundle::new(pos, 0.9, true, Visuals::sprite(MySprite::HPBox)))
}
fn missile(translation: Vec3, target: Entity) -> impl Bundle {
  (Missile { target},
   Navigation::new(130.0),
   Visuals::material_sphere(MyMaterial::GlowyMaterial3),
   SpatialBundle::from_transform(Transform::from_scale(Vec3::splat(0.2))
                                 .with_translation(translation)),
  )
}
#[derive(Component)]
pub struct Missile {
  target: Entity
}

#[derive(Component, Clone, Debug, Default)]
pub struct Inventory(HashMap<Item, u32>);

impl Inventory {
  fn add_contents(&mut self, contents: impl IntoIterator<Item = (Item, u32)>) {
    for (item, n) in contents {
      *(self.0.entry(item).or_default()) += n;
    }
  }
  fn from_contents(contents: impl IntoIterator<Item = (Item, u32)>) -> Self {
    Self(contents.into_iter().collect())
  }
}

// struct Trade {
//   pub inputs: (Item, u32),
//   pub outputs: (Item, u32)
// }
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Item {
  SpaceCat,
  Person,
  Spice,
  Coffee,
  SpaceCoin,
  Crystal,
  DiHydrogenMonoxide,
  Rock
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
#[derive(Component, Debug)]
enum Interact {
  Message(String),
  // SpaceCat,
  // SpaceStation,
  Asteroid,
  Describe,
  Item(Item),
  Trade {
    inputs: (Item, u32),
    outputs: (Item, u32)
  },
  Gate,
  Container(Vec<(Item, u32)>)
}

fn namefmt(oname: Option<&Name>) -> String {
  match oname {
    Some(name) => name.to_string(),
    None => "unnamed entity".to_string()
  }
}
const INTERACTION_RANGE: f32 = 8.0;
fn interact(mut playerq: Query<(Entity,
                   &mut Transform,
                   &GlobalTransform,
                   &mut Inventory),
                  With<Player>>,
            interactable_q: Query<(Entity, &Transform, &Interact, Option<&Name>),
                  Without<Player>>,
            gate_q: Query<(&GlobalTransform, &Gate)>,
            mut c: Commands,
            keys: Res<ButtonInput<KeyCode>>,
            mut ui_data: ResMut<UIData>) {
  if let Ok((player, mut player_transform, player_globaltransform, mut player_inventory)) =
    playerq.get_single_mut()
     && let player_pos = player_globaltransform.translation()
     && let closest_interactable_thing =
       filter_least(|tup| {
                      let dist = tup.1.translation.distance(player_pos);
                      (dist < INTERACTION_RANGE).then_some(dist as u32)
                    },
                    &interactable_q)
     && let Some((interact_entity, transform, interact, oname)) = closest_interactable_thing
  {
    let interact_message = match interact {
      Interact::Message(m) => m.to_owned(),
      Interact::Asteroid => "asteroid".to_string(),
      Interact::Describe => namefmt(oname),
      // Interact::SpaceCat => "get a space cat".to_string(),
      // Interact::SpaceStation => "talk to space station".to_string(),
      Interact::Trade { inputs: (input_item, input_number),
                        outputs: (output_item, output_number) } => {
        format!("trade {:?} {:?} for {:#?} {:#?}s",
                input_number, input_item, output_number, output_item)
      }
      Interact::Item(item) => format!("get a {:#?}", item),
      Interact::Gate => "warp".to_string(),
      Interact::Container(contents) => "take container".to_string()
    };
    ui_data.interact_message = Some(format!("[SPACE: {}]", interact_message));
    if keys.just_pressed(KeyCode::Space) {
      match interact {
        Interact::Message(m) => ui_data.message_add(m),
        Interact::Asteroid => {
          ui_data.message_add("it's an asteroid");
          ui_data.count += 1;
        }
        Interact::Describe => {
          ui_data.message_add(format!("it's a {}", namefmt(oname)));
        }
        // Interact::SpaceCat => {
        //   c.entity(interact_entity).despawn_recursive();
        //   ui_data.message_add("you found a space cat");
        //   // ui_data.space_cat_count += 1;
        // }
        // Interact::SpaceStation => {
        //   if ui_data.space_cat_count > 0 {
        //     ui_data.space_cat_count = 0;
        //     ui_data.message_add("thank you for returning these space cats");
        //   } else {
        //     ui_data.message_add("please find our space cats. they are lost");
        //   }
        // }
        &Interact::Trade { inputs: (input_item, input_number),
                           outputs: (output_item, output_number) } => {
          match player_inventory.0.get_mut(&input_item) {
            Some(mut n) if *n >= input_number => {
              *n -= input_number;
              *player_inventory.0.entry(output_item).or_default() += output_number;
              ui_data.message_add(format!("you traded {:?} {:?} for {:?} {:?}s ",
                                          input_number,
                                          input_item,
                                          output_number,
                                          output_item));
            }
            _ => ui_data.message_add("you don't have the items")
          }
        }
        &Interact::Item(item) => {
          c.entity(interact_entity).despawn_recursive();
          ui_data.message_add(format!("you got a {:#?}", item));
          *player_inventory.0.entry(item).or_default() += 1;
        }
        Interact::Gate => {
          if let Some((globaltransform, gate)) = pick(&gate_q) {
            player_transform.translation = globaltransform.translation();
          }
        }
        Interact::Container(contents) => {
          c.entity(interact_entity).despawn_recursive();
          ui_data.message_add(format!("you got things"));
          player_inventory.add_contents(contents.clone());
        }
        Interact::Item(item) => todo!(),
        Interact::Trade { inputs, outputs } => todo!()
      }
    }
  } else {
    ui_data.interact_message = None;
  }
}
// #[derive(Clone)]
// struct OverViewEntry;
// #[derive(Default, Clone)]
// pub struct OverView {
//   pub entries: Vec<OverViewEntry>
// }
// fn overview(mut c: Commands,
//             camq: Query<(Entity, &GlobalTransform), With<Camera3d>>,
//             playerq: Query<(Entity, &Player, &GlobalTransform, &Inventory)>,
//             objects_q: Query<(Entity, &SpaceObject, &GlobalTransform)>,
//             mut ui_data: ResMut<UIData>,
//             time: Res<TimeTicks>,
//             view_root_q: Query<Entity, With<ViewRoot>>) {
//   let old_overview = ui_data.overview_data.clone();
// }

fn signal_strength(player_pos: Vec3, pos: Vec3, scale: f32) -> f32 {
  scale.powi(2) / (player_pos.distance(pos)).powi(2)
}
const MESSAGE_SHOW_TIME_TICKS: u32 = 600;
fn ui(mut c: Commands,
      camq: Query<(Entity, &GlobalTransform), With<Camera3d>>,
      playerq: Query<(Entity, &Player, &GlobalTransform, &Inventory)>,
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
  if let (Ok((_, player, player_globaltransform, player_inventory)), Ok((camera, _))) =
    (playerq.get_single(), camq.get_single())
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
    let overview_max_len = 15;
    let overview_data =
      mapv(|TargetData { distance, name, .. }| format!("{name} d.:{:.1}", distance),
           take(overview_max_len,
                sort_by_key(|TargetData { distance, .. }| *distance as u32,
                            filter_map(|tup| get_target_data(tup.0), &target_q))));
    let target_data = if let Some(player_target) = player.target()
                         && let Some(TargetData { distance,
                                                  name,
                                                  ocombat,
                                                  oplanet,
                                                  .. }) = get_target_data(player_target)
    {
      filter_map(std::convert::identity,
                 [Some(format!("Target: {name}")),
                  oplanet.map(rust_utils::prettyfmt),
                  ocombat.map(|&Combat { hp, .. }| format!("hp: {hp}")),
                  Some(format!("Distance: {:.1}", distance)),
                  Some("approach: q".to_string()),
                  Some("shoot: f".to_string()),
                  Some("toggle shoot: r".to_string()),
                  Some("untarget: x".to_string())]).collect()
    } else {
      default()
    };
    let player_inventory = player_inventory.clone();
    let infobox_data =
      map(ToString::to_string, [format!("{:.1}", player_pos).as_str(),
                                "w,a,s,d,shift,ctrl: move",
                                "z: spawn mushroom man",
                                "t: target nearest hostile",
                                "g: warp",
                                "you have:"]).chain(map(|(item, n)| {
                                                          format!("{} {:?}s", n, item)
                                                        },
                                                        player_inventory.0.clone()))
                                             .collect();

    let current_time = time.0;
    let current_time_ticks = current_time;
    let message_log = rust_utils::filterv(|Message { string, time }| {
                                            time + MESSAGE_SHOW_TIME_TICKS > current_time
                                          },
                                          ui_data.message_log.clone());

    ui_data.as_mut().update(|old_ui_data| UIData { target_data,
                                                   overview_data,
                                                   current_time_ticks,
                                                   message_log,
                                                   infobox_data,
                                                   ..old_ui_data });

    if view_root_q.is_empty() {
      ui_data.message_add("message1");
      ui_data.message_add("message2");
      ui_data.message_add("message3");
      c.spawn(UIMainView.to_root());

      c.spawn(ui_root_thing_in_the_world());
    }
  }
}
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

// trait SpawnableBundle: Bundle {
//   fn spawnthisthing(self, c: &mut Commands);
// }
// impl<B: Bundle> SpawnableBundle for B {
//   fn spawnthisthing(self, c: &mut Commands) { c.spawn(self); }
// }
fn item_in_space(image: MySprite,
                 pos: Vec3,
                 scale: f32,
                 name: impl ToString,
                 item_type: Item)
                 -> impl Bundle {
  // let j: Box<dyn SpawnableBundle> = Box::new(image.clone());
  (Name::new(name.to_string()),
   Interact::Item(item_type),
   SpaceObjectBundle::new(pos, scale, true, Visuals::sprite(image)))
}

fn spawnit<B: Bundle>(c: &mut Commands, b: B) { c.spawn(b); }
#[derive(Clone, Copy, Assoc)]
#[func(pub fn spawn(&self,c:&mut Commands,pos:Vec3) -> Option<()>)]
enum Spawnable {
  Probabilistic(&'static [(f32, Spawnable)]),
  TreasureContainer,
  #[assoc(spawn = spawnit(c,space_pirate(pos)))]
  SpacePirate,
  CrystalMonster,
  SpaceMan,
  WormHole,
  #[assoc(spawn = spawnit(c,npc(pos)))]
  NPC,
  #[assoc(spawn = spawnit(c,hp_box(pos)))]
  HPBox,
  #[assoc(spawn = spawnit(c,space_coin(pos)))]
  SpaceCoin,
  TradeStation,
  #[assoc(spawn = spawnit(c,space_pirate_base(pos)))]
  SpacePirateBase,
  #[assoc(spawn = spawnit(c,asteroid(pos)))]
  Asteroid,
  #[assoc(spawn = spawnit(c,ice_asteroid(pos)))]
  IceAsteroid,
  CrystalAsteroid,
  #[assoc(spawn = spawnit(c,space_cat(pos)))]
  SpaceCat,
  SphericalCow,
  FloatingIsland,
  #[assoc(spawn = spawnit(c,space_cop(pos)))]
  SpaceCop,
  #[assoc(spawn = spawnit(c,space_wizard(pos)))]
  SpaceWizard,
  #[assoc(spawn = spawnit(c,nomad(pos)))]
  Nomad,
  #[assoc(spawn = spawnit(c,alien_soldier(pos)))]
  AlienSoldier,
  Gate,
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
impl Spawnable {
  const fn probs<const N: usize>(probs: &'static [(f32, Spawnable); N]) -> Self {
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
const NORMAL_ASTEROID_FIELD: Spawnable =
  Spawnable::probs(&[(0.5, Spawnable::Asteroid),
                     (0.1, Spawnable::CrystalAsteroid),
                     (0.1, Spawnable::IceAsteroid),
                     (0.1, Spawnable::SpaceCoin),
                     (0.5, Spawnable::CrystalMonster),
                     (1.0, Spawnable::SpaceCat)]);
const VARIOUS_ASTEROIDS: Spawnable = Spawnable::probs(&[(0.5, Spawnable::Asteroid),
                                                        (0.4, Spawnable::CrystalAsteroid),
                                                        (1.0, Spawnable::IceAsteroid)]);
const ICE_ASTEROID_FIELD: Spawnable = Spawnable::probs(&[(0.6, Spawnable::IceAsteroid),
                                                         (0.1, Spawnable::Asteroid),
                                                         (0.1, Spawnable::CrystalAsteroid),
                                                         (1.0, Spawnable::AbandonedShip)]);
const NON_COMBAT_ICE_ASTEROID_FIELD: Spawnable =
  Spawnable::probs(&[(0.6, ICE_ASTEROID_FIELD),
                     (0.5, NON_COMBAT_ZONE_THINGS),
                     (1.0, NON_HOSTILE_NPCS)]);
const PIRATE_ICE_ASTEROID_FIELD: Spawnable = Spawnable::probs(&[(0.6, ICE_ASTEROID_FIELD),
                                                                (0.5, COMBAT_ZONE_THINGS),
                                                                (1.0,
                                                                 SPACE_PIRATE_ZONE_THINGS)]);

const NON_HOSTILE_NPCS: Spawnable = Spawnable::probs(&[(0.3, Spawnable::NPC),
                                                       (0.3, Spawnable::SpaceWizard),
                                                       (0.4, Spawnable::Nomad),
                                                       (1.0, Spawnable::SpaceCop)]);
const TRADING_ZONE: Spawnable = Spawnable::probs(&[(0.7, Spawnable::TradeStation),
                                                   (1.0, Spawnable::AbandonedShip)]);
const COMBAT_ZONE_THINGS: Spawnable = Spawnable::probs(&[(0.5, Spawnable::HPBox),
                                                         (0.2,
                                                          Spawnable::TreasureContainer),
                                                         (0.1, Spawnable::AbandonedShip),
                                                         (0.3, Spawnable::SpaceMan),
                                                         (1.0, Spawnable::SpaceCoin)]);
const NON_COMBAT_ZONE_THINGS: Spawnable =
  Spawnable::probs(&[(0.1, Spawnable::SpaceCoin),
                     (0.1, Spawnable::FloatingIsland),
                     (0.1, Spawnable::SphericalCow),
                     (0.1, Spawnable::SpaceCat),
                     (1.0, Spawnable::SpaceCoin)]);

const SPACE_PIRATE_ZONE_THINGS: Spawnable =
  Spawnable::probs(&[(0.6, Spawnable::SpacePirate),
                     (0.1, Spawnable::TreasureContainer),
                     (1.0, Spawnable::SpacePirateBase)]);
const INVADERS: Spawnable = Spawnable::probs(&[(1.0, Spawnable::AlienSoldier)]);

const ANOMALY_ZONE: Spawnable = Spawnable::probs(&[(0.5, Spawnable::BlackHole),
                                                   (0.3, Spawnable::WormholePortal),
                                                   (1.0, Spawnable::QuantumAnomaly)]);

const EXOTIC_LIFE_ZONE: Spawnable = Spawnable::probs(&[(0.4, Spawnable::SpaceJellyfish),
                                                       (0.3, Spawnable::SpaceWhale),
                                                       (0.2, Spawnable::AlienArtifact),
                                                       (1.0, Spawnable::SpaceCat)]);

// Probs spawn probability sets
const NEBULA_ZONE: Spawnable = Spawnable::probs(&[(1.0, Spawnable::NebulaCloud),
                                                  (0.3, Spawnable::SolarFlare),
                                                  (0.2, Spawnable::SpaceJellyfish),
                                                  (1.0, Spawnable::IonStorm)]);

const SALVAGE_ZONE: Spawnable = Spawnable::probs(&[(0.7, Spawnable::SpaceDebris),
                                                   (0.4, Spawnable::AbandonedShip),
                                                   (0.3, Spawnable::SalvageYard),
                                                   (1.0, Spawnable::RepairDrone)]);

const ANCIENT_ZONE: Spawnable = Spawnable::probs(&[(0.6, Spawnable::AncientRuins),
                                                   (0.3, Spawnable::AlienArtifact),
                                                   (0.2, Spawnable::DysonSphere),
                                                   (1.0, Spawnable::StarforgeRemnant)]);

const FRONTIER_ZONE: Spawnable = Spawnable::probs(&[(0.5, Spawnable::AlienOutpost),
                                                    (0.3, Spawnable::FuelDepot),
                                                    (0.2, Spawnable::SpaceHermit),
                                                    (1.0, Spawnable::BountyHunter)]);

const COSMIC_HAZARD_ZONE: Spawnable = Spawnable::probs(&[(0.4, Spawnable::GravityWell),
                                                         (0.3, Spawnable::TimeDistortion),
                                                         (0.2, Spawnable::VoidEchoes),
                                                         (1.0, Spawnable::DimensionalRift)]);

const EXOTIC_ECOSYSTEM: Spawnable = Spawnable::probs(&[(0.4, Spawnable::SpaceBarnacle),
                                                       (0.3, Spawnable::CosmicSpore),
                                                       (0.2, Spawnable::SpacePiranhas),
                                                       (1.0, Spawnable::LivingCrystal)]);

const HIGH_TECH_ZONE: Spawnable = Spawnable::probs(&[(0.4, Spawnable::QuantumComputer),
                                                     (0.3, Spawnable::NanoswarmCloud),
                                                     (0.2, Spawnable::HolographicDecoy),
                                                     (1.0, Spawnable::PsiOrbNetwork)]);

const COSMIC_PHENOMENON: Spawnable = Spawnable::probs(&[(0.3, Spawnable::SpaceMirage),
                                                        (0.3,
                                                         Spawnable::CosmicStringFragment),
                                                        (0.2, Spawnable::DarkMatterNode),
                                                        (1.0, Spawnable::TachyonField)]);

const MEGAFAUNA_ZONE: Spawnable = Spawnable::probs(&[(0.4, Spawnable::AsteroidHatcher),
                                                     (0.3, Spawnable::SpaceLeviathan),
                                                     (0.2, Spawnable::VoidKraken),
                                                     (1.0, Spawnable::CrystallineEntity)]);
const SPACE_PIRATE_ASTEROID_FIELD: Spawnable =
  Spawnable::probs(&[(0.5, NORMAL_ASTEROID_FIELD),
                     (0.2, COMBAT_ZONE_THINGS),
                     (1.0, SPACE_PIRATE_ZONE_THINGS)]);

// ... (previous code remains unchanged)

const INVADER_ATTACK: Spawnable = Spawnable::probs(&[(0.4, NORMAL_ASTEROID_FIELD),
                                                     (0.1, COMBAT_ZONE_THINGS),
                                                     (0.3, NON_HOSTILE_NPCS),
                                                     (0.2, Spawnable::WormHole),
                                                     (0.1, Spawnable::SpaceStation),
                                                     (1.0, INVADERS)]);

const TRADING_ZONE_PROBS: Spawnable = Spawnable::probs(&[(0.2, NORMAL_ASTEROID_FIELD),
                                                         (0.4, TRADING_ZONE),
                                                         (1.0, NON_HOSTILE_NPCS)]);

const ASTEROID_FIELD_PROBS: Spawnable = Spawnable::probs(&[(0.4, NORMAL_ASTEROID_FIELD),
                                                           (0.2, COMBAT_ZONE_THINGS),
                                                           (1.0, NON_HOSTILE_NPCS)]);

const SPACE_STATION_ZONE_PROBS: Spawnable = Spawnable::probs(&[(0.2,
                                                                Spawnable::SpaceStation),
                                                               (0.5, VARIOUS_ASTEROIDS),
                                                               (0.5, NON_HOSTILE_NPCS),
                                                               (1.0, TRADING_ZONE)]);

const ANOMALY_CLUSTER_PROBS: Spawnable = Spawnable::probs(&[(1.0, ANOMALY_ZONE),
                                                            (0.2, COMBAT_ZONE_THINGS),
                                                            (1.0, EXOTIC_LIFE_ZONE)]);

const EXOTIC_LIFE_ZONE_PROBS: Spawnable = Spawnable::probs(&[(0.8, EXOTIC_LIFE_ZONE),
                                                             (0.3, NORMAL_ASTEROID_FIELD),
                                                             (0.1, ANOMALY_ZONE)]);

const MINEFIELD_ZONE_PROBS: Spawnable =
  Spawnable::probs(&[(0.7, Spawnable::probs(&[(1.0, Spawnable::SpaceMine)])),
                     (0.5, COMBAT_ZONE_THINGS),
                     (0.3, SPACE_PIRATE_ZONE_THINGS)]);

#[derive(Assoc, Copy, Clone, Hash, Eq, PartialEq, Debug)]
#[func(pub fn probs(&self) -> Spawnable)]
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
#[derive(Component)]
struct Gate;
fn asteroid_scale() -> f32 { rangerand(0.8, 2.3) }
fn random_normalized_vector() -> Vec3 { random::<Quat>() * Vec3::X }
fn prob(p: f32) -> bool { p > rand::random::<f32>() }

// SpaceMan,
#[derive(Component, Debug)]
pub struct Zone {
  pub faction_control: Option<Faction>,
  pub zone_radius: f32,
  pub zone_type: ZoneType,
  //pub number_of_things: u32,
  pub planet_type: Option<PlanetType>
}
impl Zone {
  fn spawn(&self, mut c: &mut Commands, zone_pos: Vec3) {
    let num_objects = 60;
    c.spawn(sign(zone_pos, prettyfmt(self)));
    for _ in 0..num_objects {
      let object_pos =
        zone_pos + (random_normalized_vector() * self.zone_radius * rangerand(0.5, 1.0));
      // let spawn_probs = self.spawn_probs();
      if let Some(spawnable) = self.zone_type.probs().pick() {
        match spawnable {
          Spawnable::TreasureContainer => {
            c.spawn(container(object_pos, [(Item::SpaceCoin, 4), (Item::Coffee, 1)]));
          }
          Spawnable::CrystalMonster => {
            c.spawn((name("crystal monster"),
                     Interact::Describe,
                     SpaceObjectBundle::new(object_pos,
                                            1.7,
                                            true,
                                            Visuals::sprite(MySprite::CrystalMonster))));
          }
          Spawnable::SpaceMan => {
            c.spawn(space_man(object_pos));
          }
          Spawnable::SphericalCow => {
            c.spawn((name("spherical cow"),
                     Interact::Describe,
                     SpaceObjectBundle::new(object_pos,
                                            1.7,
                                            true,
                                            Visuals::sprite(MySprite::SphericalCow))));
          }

          Spawnable::TradeStation => {
            let (trade, text) = if prob(0.5) {
              let trade_buy =
                pick([Item::DiHydrogenMonoxide, Item::Crystal, Item::SpaceCat]).unwrap();
              (Interact::Trade { inputs: (trade_buy, 1),
                                 outputs: (Item::SpaceCoin, 5) },
               format!("space station\nbuys {:?}", trade_buy))
            } else {
              let trade_sell = pick([Item::Spice, Item::Coffee, Item::Rock]).unwrap();
              (Interact::Trade { inputs: (Item::SpaceCoin, 5),
                                 outputs: (trade_sell, 1) },
               format!("space station\nsells {:?}", trade_sell))
            };
            c.spawn((name("space station"),
                       CanBeFollowedByNPC,
                       trade,
                       SpaceObjectBundle::new(object_pos,
                                              3.0,
                                              false,
                                              Visuals::sprite(MySprite::SpaceStation)
                                              .with_text(text))));
          }
          Spawnable::FloatingIsland => {
            c.spawn((name("floating island"),
                     Interact::Describe,
                     SpaceObjectBundle::new(object_pos,
                                            3.4,
                                            false,
                                            Visuals::sprite(MySprite::FloatingIsland))));
          }
          Spawnable::Gate => {
            c.spawn((name("gate"),
                       Interact::Gate,
                       Gate,
                       SpaceObjectBundle::new(object_pos,
                                              2.1,
                                              false,
                                              Visuals::sprite(MySprite::Gate).with_text(format!("warp gate to {}",random_zone_name()) ))));
          }
          Spawnable::NPC => {
            c.spawn(npc(object_pos));
          }
          Spawnable::WormHole => {
            c.spawn(wormhole(object_pos));
          }
          Spawnable::SpacePirateBase => {
            c.spawn(space_pirate_base(object_pos));
          }
          Spawnable::SpaceStation => {
            c.spawn(space_station(object_pos));
          }
          Spawnable::SpacePirate => {
            c.spawn(space_pirate(object_pos));
          }
          Spawnable::AbandonedShip => {
            println(format!("spawned abandoned ship at {object_pos}"));
            c.spawn((Interact::Describe,
                     name("abandoned ship"),
                     SpaceObjectBundle::new(object_pos,
                                            2.0,
                                            false,
                                            Visuals::sprite(MySprite::SpaceshipAbandoned))));
          }
          Spawnable::Asteroid => {
            c.spawn(asteroid(object_pos));
          }
          Spawnable::SpaceCat => {
            c.spawn(space_cat(object_pos));
          }
          Spawnable::IceAsteroid => {
            c.spawn(ice_asteroid(object_pos));
          }
          Spawnable::CrystalAsteroid => {
            c.spawn(crystal_asteroid(object_pos));
          }
          Spawnable::SpaceCop => {
            c.spawn(space_cop(object_pos));
          }
          Spawnable::SpaceWizard => {
            c.spawn(space_wizard(object_pos));
          }
          Spawnable::Nomad => {
            c.spawn(nomad(object_pos));
          }
          Spawnable::AlienSoldier => {
            c.spawn(alien_soldier(object_pos));
          }
          Spawnable::HPBox => {
            c.spawn(hp_box(object_pos));
          }
          Spawnable::SpaceCoin => {
            c.spawn(space_coin(object_pos));
          }
          other => println("tried to spawn {other}")
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
    Zone { zone_radius: 60.0,
           faction_control: None,
           zone_type,
           planet_type }.spawn(&mut c, zone_pos);
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
      timed_animation_system,
      combat,
      explosion_system,
      player_target_interaction,
      enemy_shoot_player,
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
