cstyles:dialog {
    label = "����������ʽ����" ;
    spacer;
    :row {
        label = "������ʽ" ;
        :column {
            spacer;
            :row {
                spacer;
                :column {
                    label = "�Ѵ��ڵ�������ʽ" ;
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
                            label = "�½���ʽ" ;
                            width = 8 ;
                        }
                        :button {
                            alignment = top ;
                            fixed_width = true ;
                            height = 2.2 ;
                            key = "btStyleRename" ;
                            label = "������" ;
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
                    label = "��Ҫ�޸ĵ�������ʽ" ;
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
                            label = "ɾ��" ;
                            width = 8 ;
                        }
                        :button {
                            alignment = top ;
                            fixed_width = true ;
                            height = 2.2 ;
                            key = "btRemoveAll" ;
                            label = "���" ;
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
        label = "ѡ����ʽ�����û��޸ĺ���ʽ������" ;
        :row {
            spacer;
            :column {
                spacer;
                :column {
                    :row {
                        label = "����" ;
                        :column {
                            children_fixed_width = true ;
                            :row {
                                children_fixed_width = true ;
                                :edit_box {
                                    allow_accept = true ;
                                    edit_width = 12 ;
                                    fixed_width = true ;
                                    key = "DCLFontFile" ;
                                    label = "SHX��������:" ;
                                    value = "hztxt_e.shx" ;
                                }
                                spacer;
                                :edit_box {
                                    allow_accept = true ;
                                    edit_width = 12 ;
                                    fixed_width = true ;
                                    key = "DCLBigFontFile" ;
                                    label = "����������:" ;
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
                            label = "Ч��" ;
                            :column {
                                children_fixed_width = true ;
                                :row {
                                    children_fixed_width = true ;
                                    :column {
                                        children_fixed_width = true ;
                                        :toggle {
                                            key = "DCLdiandao" ;
                                            label = "�ߵ�" ;
                                            value = "0" ;
                                        }
                                        :toggle {
                                            key = "DCLfanxiang" ;
                                            label = "����" ;
                                            value = "0" ;
                                        }
                                        :toggle {
                                            key = "DCLChuiZhi" ;
                                            label = "��ֱ" ;
                                            value = "0" ;
                                        }
                                    }
                                    spacer;
                                    :column {
                                        children_fixed_width = true ;
                                        :edit_box {
                                            key = "DCLHeight" ;
                                            label = "���ָ߶�:" ;
                                            value = "3" ;
                                        }
                                        :edit_box {
                                            key = "DCLWidth" ;
                                            label = "��ȱ���:" ;
                                            value = "0.8" ;
                                        }
                                        :edit_box {
                                            key = "DCLObliqueAngle" ;
                                            label = "��б�Ƕ�:" ;
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
                                label = "ʹ�÷��ε�������" ;
                                width = 8 ;
                            }
                            :button {
                                alignment = top ;
                                fixed_width = true ;
                                height = 2.2 ;
                                key = "btSetSongTi" ;
                                label = "ʹ����������" ;
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
        label = "��Ȩ���У�  ����ȫ" ;
    }
    :text {
        label = "��ϵ��ʽ��  QQ:32774362   Email:guangdonglbq@qq.com" ;
    }
    :text {
        label = "2008��4��18��  ��  �㶫��ɽ" ;
    }
    ok_cancel;
}