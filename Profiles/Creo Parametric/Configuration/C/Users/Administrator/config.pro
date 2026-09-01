!English version
dialog_translation no
help_translation no
msg_translation no
menu_translation yes

pro_unit_length UNIT_MM
pro_unit_mass UNIT_KILOGRAM

!Fonts
!default_ui_font 微软雅黑
!menu_font 微软雅黑
!model_tree_font 微软雅黑
!popuphelp_font 微软雅黑
pro_font_dir C:\Windows\Fonts

!system colors

system_colors_file $PRO_DIRECTORY\iaso2h\text\system_colors\syscol.scl

!Default Path
pro_format_dir $PRO_DIRECTORY\iaso2h\formats
pro_symbol_dir $PRO_DIRECTORY\iaso2h\symbol
start_model_dir $PRO_DIRECTORY\iaso2h\templates

!Drawing & format setup files
drawing_setup_file $PRO_DIRECTORY\iaso2h\text\drawing.dtl
format_setup_file $PRO_DIRECTORY\iaso2h\text\format.dtl

!Template models directory
template_new_ecadasm $PRO_DIRECTORY\templates\inlbs_ecad_asm_abs.asm
template_boardpart $PRO_DIRECTORY\templates\inlbs_ecad_board_abs.prt
template_drawing $PRO_DIRECTORY\templates\c_drawing.drw
template_designasm $PRO_DIRECTORY\iaso2h\templates\start_asm.asm
template_sheetmetalpart $PRO_DIRECTORY\iaso2h\templates\start_sheet.prt
template_solidpart $PRO_DIRECTORY\iaso2h\templates\start_part.prt
template_harnesspart $PRO_DIRECTORY\iaso2h\templates\start_part.prt
template_flat_harness $PRO_DIRECTORY\iaso2h\templates\mmks_flat_harness.asm
template_mfgnc $PRO_DIRECTORY\iaso2h\templates\mmks_mfg_nc.asm
template_mfgmold $PRO_DIRECTORY\iaso2h\templates\mmks_mfg_mold.asm

!trail_dir D:\ptc\temp\Creotemp8.0
!train_file_path D:\ptc\temp\Creotemp8.0
!pro_group_dir $PRO_DIRECTORY\iaso2h\group
pro_note_dir $PRO_DIRECTORY\iaso2h\notes
!pro_library_dir $PRO_DIRECTORY\iaso2h\library
pro_material_dir $PRO_DIRECTORY\iaso2h\text\materials-library
pro_table_dir $PRO_DIRECTORY\iaso2h\text\table
!mdl_tree_cfg_file $PRO_DIRECTORY\iaso2h\config\tree.cfg
!pro_catalog_dir $PRO_DIRECTORY\iaso2h\library
!pro_pip_lnstk_dir $PRO_DIRECTORY\iaso2h\linestock
pro_plot_config_dir $PRO_DIRECTORY\iaso2h\text\plot_config



search_path_file $CREO_COMMON_FILES\ifx\parts\prolibrary\search.pro

!Layer Setup
intf2d_out_dxf_mapping_file $PRO_DIRECTORY\iaso2h\intf_configs\dxf_export.pro
def_layer LAYER_DATUM               DATUM
def_layer LAYER_AXIS                AXIS
def_layer LAYER_CSYS                CSYS
def_layer LAYER_POINT               POINT
def_layer LAYER_CURVE               CURVE
def_layer LAYER_QUILT               QUILT
def_layer layer_surface 	    SURFS
def_layer layer_hole_feat 	    HOLES
def_layer layer_draft_feat 	    DRAFTS
def_layer layer_round_feat 	    ROUNDS
def_layer layer_chamfer_feat 	    CHAMFER
def_layer layer_cosm_sketch 	    COSMETICS
def_layer layer_copy_geom_feat 	    COPY_GEOM
def_layer layer_skeleton_model      skeleton
def_layer layer_detail_item 	    DETAILS
def_layer layer_draft_dim 	    DRAFT_DIMS
def_layer layer_refdim 		    REF_DIMS
def_layer layer_gtol 		    GTOLS
def_layer layer_symbol 		    SYMBOLS
def_layer layer_note 		    NOTES
def_layer layer_dwg_table 	    TABLES
def_layer layer_draft_geom 	    DRAFT_GEOM


todays_date_note_format %Y-%m-%d
tolerance_standard iso
weld_ui_standard iso
max_animation_time 0.7
update_old_appearance ALWAYS
visible_annotations_scope ACTIVE MODEL ONLY
visible_mapkeys YES
visible_recent_files 30
smooth_lines YES
save_display YES
show_old_feature_statuses NO
show_selected_item_id YES
show_sketcher_constr_dyn_edit NO
show_sketch_dims_in_feature YES
sketcher_undo_reorient_view YES
spin_with_silhouettes YES
use_inverted_mousewheel_zoom yes
spin_center_display no
display_axes yes
display_axis_tags no
display_coord_sys no
display_coord_sys_tags no
display_planes yes
display_plane_tags yes
display_points yes
display_point_tags no
sketcher_highlight_intersecting yes
sketcher_highlight_junctions yes
sketcher_highlight_overlapping yes
bell no
selection_prioritize_quilts yes
default_boundary_refs_strong yes
display shadewithedges
save_texture_with_model yes
allow_confirm_window yes
prompt_on_exit yes
retain_display_memory yes
visible_message_lines 2
preferred_export_format dwg
dxf_export_format 2010
dxf_out_drawing_scale yes
dxf_out_scale_views yes
dxf_out_comments yes
dxf_out_sep_dim_w_breaks no
intf2d_out_acad_mtext yes
intf2d_out_acad_text_align as_is
intf2d_out_acad_splines as_spline
intf2d_out_acad_hatches as_hatch
intf2d_out_acad_ellipses yes
intf2d_out_line_width yes
intf_out_layer block_layer
save_drawing_picture_file embed
intf2d_out_pnt_ent yes
dxf_block_to_pro_symbol yes
intf2d_out_acad_unicode yes
intf_use_variable_size yes
auto_associate_dimensions yes
allow_move_view_with_move yes
highlight_new_dims yes
parenthesize_ref_dim yes
tol_display no
display_annotations yes
smt_bend_notes_dflt_display yes
display_full_object_path no
sketcher_disp_constraints yes
sketcher_disp_dimensions yes
default_ang_dec_places 2
default_dec_places 2
enable_2x_traj_multi_loops yes
foreground_curves yes
shade_with curves
enable_insep_asm_operations yes

! Map Keys
dwg_sketch_parallel_guide yes
dwg_sketch_horizontal_guide no
dwg_sketch_vertical_guide no
dwg_sketch_perpendicular_guide no
dwg_sketch_midpoint_guide no
dwg_sketch_tangent_guide no
dwg_sketch_coincident_guide no
