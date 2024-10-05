use {crate::PlayerTargetInteractionState,
     bevy::{math::Vec3,
            prelude::*},
     bevy_quill::{prelude::*, Portal, ViewChild},
     bevy_quill_overlays::{Overlay, PolygonOptions, StrokeMarker},
     rust_utils::{concat_strings, MutateTrait}};

const UI_BACKGROUND_COLOR: Color = Color::srgba(0.0, 0.0, 0.0, 0.5);
const UI_BORDER_COLOR: Color = Color::srgba(0.0, 0.0, 0.0, 0.7);
pub const MESSAGE_LOG_MAX_LEN: usize = 5;
#[derive(Clone, PartialEq, PartialOrd, Eq, Hash, Ord)]
pub struct Message {
  pub time: u32,
  pub string: String
}
#[derive(Default, Clone, Debug)]
pub struct TargetData {
  pub name: String,
  pub distance: f32,
  pub target_interaction_state: Option<PlayerTargetInteractionState>
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

impl UIData {
  pub fn message_add(&mut self, message: impl ToString) {
    let time = self.current_time_ticks;
    self.message_log.push(Message { string: message.to_string(),
                                    time });
  }
}
fn style_button(ss: &mut StyleBuilder) {
  ss.border(1)
    .display(bevy::ui::Display::Flex)
    .flex_direction(bevy::ui::FlexDirection::Row)
    .justify_content(bevy::ui::JustifyContent::Center)
    .align_items(bevy::ui::AlignItems::Center)
    .align_content(bevy::ui::AlignContent::Center)
    .padding((12, 0))
    .border(0);
}

pub fn viewchild(view: impl View + 'static) -> ViewChild { ViewChild::new(view) }
#[derive(Clone, PartialEq)]
pub struct MessageLogView;
impl ViewTemplate for MessageLogView {
  type View = impl View;
  fn create(&self, cx: &mut Cx) -> Self::View {
    let ui_data = cx.use_resource::<UIData>();
    let style = |sb: &mut StyleBuilder| {
      sb.flex_direction(FlexDirection::ColumnReverse)
        .left(0)
        .bottom(0);
    };
    let children = rust_utils::concat_strings(ui_data.message_log
                                                     .clone()
                                                     .mutate(|v| v.sort())
                                                     .into_iter()
                                                     .map(|msg| msg.string)
                                                     .intersperse("\n".to_string()));
    Portal::new(Element::<NodeBundle>::new().style((common_style, style))
                                            .children(children))
  }
}
fn intersperse_newline<T: ToString>(coll: impl IntoIterator<Item = T>) -> String {
  concat_strings(coll.into_iter()
                     .map(|v| v.to_string())
                     .intersperse("\n".to_string()))
}
fn cond_view<V: View>(b: bool, v: V) -> Cond<V, ()> { Cond::new(b, v, ()) }

fn option_view<V: View + Default>(v: Option<V>) -> Cond<V, ()> {
  Cond::new(v.is_some(), v.unwrap_or_default(), ())
}
#[derive(Clone, PartialEq)]
pub struct InteractPopUp;
impl ViewTemplate for InteractPopUp {
  type View = impl View;
  fn create(&self, cx: &mut Cx) -> Self::View {
    // uuid::Uuid::from_subset()
    let ui_data = cx.use_resource::<UIData>();
    let style = |sb: &mut StyleBuilder| {
      sb.justify_self(JustifySelf::Center).top(Val::Percent(80.0));
    };
    let msg = ui_data.interact_message.clone();
    cond_view(msg.is_some(),
              Element::<NodeBundle>::new().style((common_style, style))
                                          .children(msg.unwrap_or_default()))
  }
}
#[derive(Clone, PartialEq)]
pub struct InfoBox;
impl ViewTemplate for InfoBox {
  type View = impl View;
  fn create(&self, cx: &mut Cx) -> Self::View {
    let infobox_data = cx.use_resource::<UIData>().infobox_data.clone();
    let style = |sb: &mut StyleBuilder| {
      sb.left(0).top(0)// .flex_column()
        ;
    };
    Element::<NodeBundle>::new().style((common_style, style))
                                .children(intersperse_newline(infobox_data))
  }
}
#[derive(Clone, PartialEq)]
pub struct TargetPopUp;
impl ViewTemplate for TargetPopUp {
  type View = impl View;
  fn create(&self, cx: &mut Cx) -> Self::View {
    let target_data = cx.use_resource::<UIData>().target_data.clone();
    let text = intersperse_newline(target_data);
    let style = |sb: &mut StyleBuilder| {
      sb.bottom(0).right(0);
    };
    Element::<NodeBundle>::new().style((common_style, style))
                                .children(text)
  }
}
#[derive(Clone, PartialEq)]
pub struct OverViewPopUp;
impl ViewTemplate for OverViewPopUp {
  type View = impl View;
  fn create(&self, cx: &mut Cx) -> Self::View {
    let overview_data = cx.use_resource::<UIData>().overview_data.clone();
    let text = intersperse_newline(overview_data);
    let style = |sb: &mut StyleBuilder| {
      sb.top(0).right(0);
    };
    Element::<NodeBundle>::new().style((common_style, style))
                                .children(text)
  }
}
/// Example of a view template that displays a string.
#[derive(Clone, PartialEq)]
pub struct OverlayExample;
impl ViewTemplate for OverlayExample {
  type View = impl View;
  fn create(&self, _cx: &mut Cx) -> Self::View {
    let ui_data = _cx.use_resource::<UIData>();
    let trans = Transform::from_translation(Vec3::new(0., 1000.0, 0.))
      .with_rotation(Quat::from_rotation_y(-std::f32::consts::PI * 0.3 * (ui_data.count as f32)));
    Overlay::new().shape(|sb| {
                    sb.with_stroke_width(0.3)
                      // .with_orientation(ShapeOrientation::YPositive)
                      .stroke_rect(Rect::from_center_size(Vec2::new(0., 0.),
                                                          Vec2::new(2., 2.)))
                      .fill_rect(Rect::from_center_size(Vec2::new(3., 0.),
                                                        Vec2::new(2., 2.)))
                      .stroke_circle(Vec2::new(0., 3.), 0.7, 32)
                      .fill_circle(Vec2::new(3., 3.), 0.7, 32)
                      .fill_triangle(Vec2::new(-3., 3.),
                                     Vec2::new(-2., 3.),
                                     Vec2::new(-3., 4.))
                      .fill_quad(Vec2::new(-3., 6.),
                                 Vec2::new(-2., 6.),
                                 Vec2::new(-2., 8.),
                                 Vec2::new(-3., 7.))
                      .stroke_polygon(&[Vec2::new(3., -2.),
                                        Vec2::new(2., -2.),
                                        Vec2::new(2., -4.),
                                        Vec2::new(3., -5.)],
                                      PolygonOptions::default())
                      .stroke_polygon(&[Vec2::new(5., -2.),
                                        Vec2::new(4., -2.),
                                        Vec2::new(4., -4.),
                                        Vec2::new(5., -5.)],
                                      PolygonOptions { closed: true,
                                                       ..Default::default() })
                      .stroke_polygon(&[Vec2::new(7., -2.),
                                        Vec2::new(6., -2.),
                                        Vec2::new(6., -4.),
                                        Vec2::new(7., -5.)],
                                      PolygonOptions { start_marker:
                                                         StrokeMarker::Arrowhead,
                                                       end_marker:
                                                         StrokeMarker::Arrowhead,
                                                       ..default() });
                  })
                  .underlay(0.0)
                  .color(bevy::color::palettes::css::YELLOW)
                  .transform(trans)
                  .children("aaaaaaaa".to_string())
  }
}

pub struct WarpGui {
  index_selected: u8
}
/// Example of a view template that displays a string.
#[derive(Clone, PartialEq)]
struct ChildViewExample;

impl ViewTemplate for ChildViewExample {
  type View = impl View;
  fn create(&self, cx: &mut Cx) -> Self::View {
    let counter = cx.use_resource::<UIData>();
    format!("{}", counter.count)
  }
}

/// Example of a `Cond` view.
#[derive(Clone, PartialEq)]
struct EvenOdd;

impl ViewTemplate for EvenOdd {
  type View = impl View;
  fn create(&self, cx: &mut Cx) -> Self::View {
    let counter = cx.use_resource::<UIData>();
    Cond::new(counter.count & 1 == 0, "[Even]", "[Odd]")
  }
}

#[derive(Clone, PartialEq)]
struct NestedInner {
  count: u32
}

impl ViewTemplate for NestedInner {
  type View = impl View;
  fn create(&self, _cx: &mut Cx) -> Self::View {
    Cond::new(self.count & 1 == 0, "[E]", "[O]")
  }
}

/// Example of a view template that invokes another view template.
#[derive(Clone, PartialEq)]
struct Nested;

impl ViewTemplate for Nested {
  type View = impl View;
  fn create(&self, cx: &mut Cx) -> Self::View {
    let &UIData { count, .. } = cx.use_resource::<UIData>();
    NestedInner { count }
  }
}

#[derive(Clone, PartialEq)]
struct DynamicStyle;

impl ViewTemplate for DynamicStyle {
  type View = impl View;
  fn create(&self, cx: &mut Cx) -> Self::View {
    let counter = cx.use_resource::<UIData>();
    Element::<NodeBundle>::new().style_dyn(|ct, sb| {
                 if ct {
                   sb.border_color(bevy::color::palettes::css::RED).border(3);
                 } else {
                   sb.border_color(bevy::color::palettes::css::LIME).border(3);
                 }
               },
               counter.count & 1 == 0)
    .children("Style")
  }
}

fn style_test(sb: &mut StyleBuilder) {
  // ZIndex::try_into
  sb.display(Display::Flex)
    .flex_direction(FlexDirection::Column)
    .position(bevy::ui::PositionType::Absolute)
    // .z_index
    .padding(3)
    .left(0)
    .right(0)
    .top(0)
    .bottom(0)
    // .row_gap(4)
    // .background_color(Color::srgba(0.0, 0.0, 0.0, 0.0))
    ;
}

fn style_row(sb: &mut StyleBuilder) {
  sb.display(Display::Flex)
    .flex_direction(FlexDirection::Row)
    .align_items(bevy::ui::AlignItems::Center)
    .column_gap(4);
}

pub fn common_style(sb: &mut StyleBuilder) {
  sb.font_size(32.0)
    .display(Display::Block)
    .border(1)
    .border_color(UI_BORDER_COLOR)
    .background_color(UI_BACKGROUND_COLOR)
    .position(bevy::ui::PositionType::Absolute)
    .padding(3)
    .pointer_events(false);
}
#[derive(Clone, PartialEq)]
pub struct UIMainView;
impl ViewTemplate for UIMainView {
  type View = impl View;
  fn create(&self, cx: &mut Cx) -> Self::View {
    let click = cx.create_callback(|| {
                    info!("Clicked!");
                  });

    vec![ViewChild::new(InfoBox),
         ViewChild::new(MessageLogView),
         ViewChild::new(TargetPopUp),
         ViewChild::new(OverViewPopUp),
         ViewChild::new(InteractPopUp)]
  }
}
pub fn ui_root_thing_in_the_world() -> impl Bundle {
  Element::<SpatialBundle>::new() // .insert(TargetCamera(root))
                                 .style((style_test, style_test, style_test, style_test))
                                 .children(("Hello, ",
                                            "world! ",
                                            ChildViewExample,
                                            " ",
                                            EvenOdd,
                                            " ",
                                            Nested,
                                            DynamicStyle,
                                            OverlayExample))
                                 .to_root()
}
