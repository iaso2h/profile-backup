mapkey mm @MAPKEY_LABELRectangle;~ Command `ProCmdSketRectangle`  1;
mapkey ms @MAPKEY_LABELRectangle Slanted;\
mapkey(continued) ~ Command `ProCmdSketSlantRectangle`  1;
mapkey mc @MAPKEY_LABELRectangle Center;\
mapkey(continued) ~ Command `ProCmdSketCenterRectangle`  1;
mapkey mp @MAPKEY_LABELParallelogram;~ Command `ProCmdSketParallelogram`  1;
mapkey aa @MAPKEY_LABELArc Center;~ Command `ProCmdSketCenterEnds`  1;
mapkey a3 @MAPKEY_LABELArc 3 Points;~ Command `ProCmdSket3Point`  1;
mapkey u @MAPKEY_LABELSlots;~ Command `ProCmdSketPalette` ;\
mapkey(continued) ~ Select `sket_palette` `CategoriesTab` 1 `shapes1CategoryLayout`;\
mapkey(continued) ~ Select `sket_palette` `shapes1TemplateList` 1;
mapkey f @MAPKEY_LABELFillet;~ Command `ProCmdSketCirFilletWConstr`  1;
mapkey gf @MAPKEY_LABELFeature Filter Option;\
mapkey(continued) ~ Command `ProCmdSelFilterSet` 0;
mapkey gv @MAPKEY_LABELVertex Filter Option;~ Command `ProCmdSelFilterSet` 8;
mapkey ge @MAPKEY_LABELEdge Filter Option;~ Command `ProCmdSelFilterSet` 9;
mapkey gb @MAPKEY_LABELBody Filter Option;\
mapkey(continued) ~ Open `main_dlg_cur` `Sst_bar.filter_list`;\
mapkey(continued) ~ Close `main_dlg_cur` `Sst_bar.filter_list`;\
mapkey(continued) ~ Select `main_dlg_cur` `Sst_bar.filter_list` 1 `body_fltr`;
mapkey gs @MAPKEY_LABELSurface Filter Option;\
mapkey(continued) ~ Command `ProCmdSelFilterSet` 5;
mapkey gg @MAPKEY_LABELGeometry Filter Option;\
mapkey(continued) ~ Command `ProCmdSelFilterSet` 320002;
mapkey gr @MAPKEY_LABELRegion Filter Option;\
mapkey(continued) ~ Command `ProCmdSelFilterSet` 452;
mapkey ac @MAPKEY_LABELArc Center;~ Command `ProCmdSketCenterEnds`  1;
