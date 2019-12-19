module TestFile
let testfile =
    """[gd_scene load_steps=2 format=2]

[ext_resource path="res://controls.gd" type="Script" id=1]

[node name="controls_ui" type="Control"]
margin_right = 40.0
margin_bottom = 40.0
size_flags_horizontal = 2
size_flags_vertical = 2
script = ExtResource( 1 )
__meta__ = {
"__editor_plugin_screen__": "2D"
}

[node name="contextual_help" type="Label" parent="."]
margin_left = 100.0
margin_top = 50.0
margin_right = 465.0
margin_bottom = 89.0
size_flags_horizontal = 2
size_flags_vertical = 0
text = "Click on a key binding to reassign it, or press the Cancel action."

[node name="bindings" type="Control" parent="."]
margin_left = 50.0
margin_top = 50.0
margin_right = 90.0
margin_bottom = 90.0
size_flags_horizontal = 2
size_flags_vertical = 2

[node name="move_up" type="Control" parent="bindings"]
margin_left = 50.0
margin_top = 50.0
margin_right = 90.0
margin_bottom = 90.0
size_flags_horizontal = 2
size_flags_vertical = 2

[node name="Label" type="Label" parent="bindings/move_up"]
margin_left = 5.0
margin_top = 8.0
margin_right = 45.0
margin_bottom = 21.0
size_flags_horizontal = 2
size_flags_vertical = 0
text = "Up"

[node name="Button" type="Button" parent="bindings/move_up"]
margin_left = 84.0
margin_top = -1.0
margin_right = 144.0
margin_bottom = 29.0
size_flags_horizontal = 2
size_flags_vertical = 2

[node name="move_down" type="Control" parent="bindings"]
editor/display_folded = true
margin_left = 50.0
margin_top = 100.0
margin_right = 90.0
margin_bottom = 140.0
size_flags_horizontal = 2
size_flags_vertical = 2

[node name="Label" type="Label" parent="bindings/move_down"]
margin_left = 5.0
margin_top = 8.0
margin_right = 45.0
margin_bottom = 21.0
size_flags_horizontal = 2
size_flags_vertical = 0
text = "Down"

[node name="Button" type="Button" parent="bindings/move_down"]
margin_left = 84.0
margin_top = -1.0
margin_right = 144.0
margin_bottom = 29.0
size_flags_horizontal = 2
size_flags_vertical = 2

[node name="move_left" type="Control" parent="bindings"]
editor/display_folded = true
margin_left = 50.0
margin_top = 150.0
margin_right = 90.0
margin_bottom = 190.0
size_flags_horizontal = 2
size_flags_vertical = 2

[node name="Label" type="Label" parent="bindings/move_left"]
margin_left = 5.0
margin_top = 8.0
margin_right = 45.0
margin_bottom = 21.0
size_flags_horizontal = 2
size_flags_vertical = 0
text = "Left"

[node name="Button" type="Button" parent="bindings/move_left"]
margin_left = 84.0
margin_top = -1.0
margin_right = 144.0
margin_bottom = 29.0
size_flags_horizontal = 2
size_flags_vertical = 2

[node name="move_right" type="Control" parent="bindings"]
editor/display_folded = true
margin_left = 50.0
margin_top = 200.0
margin_right = 90.0
margin_bottom = 240.0
size_flags_horizontal = 2
size_flags_vertical = 2

[node name="Label" type="Label" parent="bindings/move_right"]
margin_left = 5.0
margin_top = 8.0
margin_right = 45.0
margin_bottom = 21.0
size_flags_horizontal = 2
size_flags_vertical = 0
text = "Right"

[node name="Button" type="Button" parent="bindings/move_right"]
margin_left = 84.0
margin_top = -1.0
margin_right = 144.0
margin_bottom = 29.0
size_flags_horizontal = 2
size_flags_vertical = 2

[node name="jump" type="Control" parent="bindings"]
editor/display_folded = true
margin_left = 50.0
margin_top = 250.0
margin_right = 90.0
margin_bottom = 290.0
size_flags_horizontal = 2
size_flags_vertical = 2

[node name="Label" type="Label" parent="bindings/jump"]
margin_left = 5.0
margin_top = 8.0
margin_right = 45.0
margin_bottom = 21.0
size_flags_horizontal = 2
size_flags_vertical = 0
text = "Jump"

[node name="Button" type="Button" parent="bindings/jump"]
margin_left = 84.0
margin_top = -1.0
margin_right = 144.0
margin_bottom = 29.0
size_flags_horizontal = 2
size_flags_vertical = 2

"""
