cstyles:dialog {
    label = "批量文字样式管理" ;
    spacer;
    :row {
        label = "文字样式" ;
        :column {
            spacer;
            :row {
                spacer;
                :column {
                    label = "已存在的文字样式" ;
                    :list_box {
                        allow_accept = true ;
                        height = 16 ;
                        key = "StylesExist" ;
                        multiple_select = true ;
                        width = 25 ;
                    }
                    :row {
                        :button {
                            alignment = top ;
                            fixed_width = true ;
                            height = 2.2 ;
                            key = "btStyleNew" ;
                            label = "新建样式" ;
                            width = 8 ;
                        }
                        :button {
                            alignment = top ;
                            fixed_width = true ;
                            height = 2.2 ;
                            key = "btStyleRename" ;
                            label = "重命名" ;
                            width = 8 ;
                        }
                    }
                    spacer;
                }
                :column {
                    children_alignment = centered ;
                    children_fixed_width = true ;
                    spacer;
                    :button {
                        alignment = top ;
                        fixed_width = true ;
                        height = 2.2 ;
                        key = "btAddSelected" ;
                        label = "-->" ;
                        width = 8 ;
                    }
                    :button {
                        alignment = top ;
                        fixed_width = true ;
                        height = 2.2 ;
                        key = "btAddAll" ;
                        label = "-->>" ;
                        width = 8 ;
                    }
                    spacer;
                }
                :column {
                    label = "需要修改的文字样式" ;
                    :list_box {
                        allow_accept = true ;
                        height = 16 ;
                        key = "StylesToBeChanged" ;
                        multiple_select = true ;
                        width = 25 ;
                    }
                    :row {
                        spacer;
                        :button {
                            alignment = top ;
                            fixed_width = true ;
                            height = 2.2 ;
                            key = "btRemoveOne" ;
                            label = "删除" ;
                            width = 8 ;
                        }
                        :button {
                            alignment = top ;
                            fixed_width = true ;
                            height = 2.2 ;
                            key = "btRemoveAll" ;
                            label = "清空" ;
                            width = 8 ;
                        }
                        spacer;
                    }
                }
            }
            spacer;
        }
        :spacer {
        }
    }
    spacer;
    :column {
        label = "选定样式的设置或修改后样式的设置" ;
        :row {
            spacer;
            :column {
                spacer;
                :column {
                    :row {
                        label = "字体" ;
                        :column {
                            children_fixed_width = true ;
                            :row {
                                children_fixed_width = true ;
                                :edit_box {
                                    allow_accept = true ;
                                    edit_width = 12 ;
                                    fixed_width = true ;
                                    key = "DCLFontFile" ;
                                    label = "SHX字体名称:" ;
                                    value = "hztxt_e.shx" ;
                                }
                                spacer;
                                :edit_box {
                                    allow_accept = true ;
                                    edit_width = 12 ;
                                    fixed_width = true ;
                                    key = "DCLBigFontFile" ;
                                    label = "大字体名称:" ;
                                    value = "hztxt.shx" ;
                                }
                            }
                            spacer;
                        }
                        spacer;
                    }
                    spacer;
                    :row {
                        :row {
                            children_fixed_width = true ;
                            label = "效果" ;
                            :column {
                                children_fixed_width = true ;
                                :row {
                                    children_fixed_width = true ;
                                    :column {
                                        children_fixed_width = true ;
                                        :toggle {
                                            key = "DCLdiandao" ;
                                            label = "颠倒" ;
                                            value = "0" ;
                                        }
                                        :toggle {
                                            key = "DCLfanxiang" ;
                                            label = "反向" ;
                                            value = "0" ;
                                        }
                                        :toggle {
                                            key = "DCLChuiZhi" ;
                                            label = "垂直" ;
                                            value = "0" ;
                                        }
                                    }
                                    spacer;
                                    :column {
                                        children_fixed_width = true ;
                                        :edit_box {
                                            key = "DCLHeight" ;
                                            label = "文字高度:" ;
                                            value = "3" ;
                                        }
                                        :edit_box {
                                            key = "DCLWidth" ;
                                            label = "宽度比例:" ;
                                            value = "0.8" ;
                                        }
                                        :edit_box {
                                            key = "DCLObliqueAngle" ;
                                            label = "倾斜角度:" ;
                                            value = "0" ;
                                        }
                                    }
                                }
                                spacer;
                            }
                            spacer;
                        }
                        :spacer {
                        }
                        :column {
                            children_alignment = centered ;
                            children_fixed_width = true ;
                            spacer;
                            :button {
                                alignment = top ;
                                fixed_width = true ;
                                height = 2.2 ;
                                key = "btSetDefault" ;
                                label = "使用仿宋单笔字体" ;
                                width = 8 ;
                            }
                            :button {
                                alignment = top ;
                                fixed_width = true ;
                                height = 2.2 ;
                                key = "btSetSongTi" ;
                                label = "使用宋体字体" ;
                                width = 8 ;
                            }
                            spacer;
                        }
                    }
                }
                spacer;
            }
            spacer;
        }
    }

    spacer;
    :text {
        label = "版权所有：  梁伯全" ;
    }
    :text {
        label = "联系方式：  QQ:32774362   Email:guangdonglbq@qq.com" ;
    }
    :text {
        label = "2008年4月18日  于  广东中山" ;
    }
    ok_cancel;
}