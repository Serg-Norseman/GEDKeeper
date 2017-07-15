using System;
using Eto.Drawing;
using Eto.Forms;
using GKUI.Components;

namespace GKUI
{
    partial class TreeChartWin
    {
        private ToolBar ToolBar1;
        private ButtonToolItem tbImageSave;
        private ContextMenu MenuPerson;
        private ButtonMenuItem miEdit;
        private ButtonMenuItem miSpouseAdd;
        private ButtonMenuItem miSonAdd;
        private ButtonMenuItem miDaughterAdd;
        private ButtonMenuItem miFamilyAdd;
        private ButtonMenuItem miDelete;
        private ButtonMenuItem miRebuildKinships;
        private ButtonToolItem tbModes;
        private ContextMenu MenuModes;
        private RadioMenuItem miModeBoth;
        private RadioMenuItem miModeAncestors;
        private RadioMenuItem miModeDescendants;
        private CheckMenuItem miTraceSelected;
        private CheckMenuItem miTraceKinships;
        private CheckMenuItem miCertaintyIndex;
        private ButtonMenuItem miRebuildTree;
        private ButtonToolItem tbGens;
        private ContextMenu MenuGens;
        private RadioMenuItem miGensInf;
        private RadioMenuItem miGens1;
        private RadioMenuItem miGens2;
        private RadioMenuItem miGens3;
        private RadioMenuItem miGens4;
        private RadioMenuItem miGens5;
        private RadioMenuItem miGens6;
        private RadioMenuItem miGens7;
        private RadioMenuItem miGens8;
        private RadioMenuItem miGens9;
        private ButtonMenuItem miFillColor;
        private ButtonMenuItem miFillImage;
        private ButtonMenuItem miFatherAdd;
        private ButtonMenuItem miMotherAdd;

        private ButtonToolItem tbDocPrint;
        private ButtonToolItem tbDocPreview;
        private ButtonToolItem tbFilter;
        private ButtonToolItem tbPrev;
        private ButtonToolItem tbNext;
        private ButtonToolItem tbOptions;

        private void InitializeComponent()
        {
            SuspendLayout();

            tbImageSave = new ButtonToolItem();
            tbImageSave.Click += tbImageSave_Click;
            tbImageSave.Image = Bitmap.FromResource("Resources.btn_save_image.gif");

            tbGens = new ButtonToolItem();
            //tbGens.DropDown = MenuGens;
            tbGens.Text = "tbGens";
            tbGens.Click  += (sender, e) => MenuGens.Show(this);

            tbModes = new ButtonToolItem();
            //tbModes.DropDown = MenuModes;
            tbModes.Click  += (sender, e) => MenuModes.Show(this);
            tbModes.Image = Bitmap.FromResource("Resources.btn_tools.gif");

            tbFilter = new ButtonToolItem();
            tbFilter.Click += ToolBar1_ButtonClick;
            tbFilter.Image = Bitmap.FromResource("Resources.btn_filter.gif");

            tbPrev = new ButtonToolItem();
            tbPrev.Enabled = false;
            tbPrev.Click += ToolBar1_ButtonClick;
            tbPrev.Image = Bitmap.FromResource("Resources.btn_left.gif");

            tbNext = new ButtonToolItem();
            tbNext.Enabled = false;
            tbNext.Click += ToolBar1_ButtonClick;
            tbNext.Image = Bitmap.FromResource("Resources.btn_right.gif");

            tbDocPreview = new ButtonToolItem();
            tbDocPreview.Click += tbDocPreview_Click;
            tbDocPreview.Image = Bitmap.FromResource("Resources.btn_preview.gif");

            tbDocPrint = new ButtonToolItem();
            tbDocPrint.Click += tbDocPrint_Click;
            tbDocPrint.Image = Bitmap.FromResource("Resources.btn_print.gif");

            tbOptions = new ButtonToolItem();
            tbOptions.Click += tbOptions_Click;
            tbOptions.Image = Bitmap.FromResource("Resources.btn_tools.gif");

            ToolBar1 = new ToolBar();
            ToolBar1.TextAlign = ToolBarTextAlign.Right;
            ToolBar1.Items.AddRange(new ToolItem[] {
                                        tbImageSave,
                                        new SeparatorToolItem(),
                                        tbGens,
                                        new SeparatorToolItem(),
                                        tbModes,
                                        new SeparatorToolItem(),
                                        tbFilter,
                                        tbPrev,
                                        tbNext,
                                        new SeparatorToolItem(),
                                        tbDocPreview,
                                        tbDocPrint,
                                        new SeparatorToolItem(),
                                        tbOptions});

            miGensInf = new RadioMenuItem();
            miGensInf.Checked = true;
            miGensInf.Text = "Inf";
            miGensInf.Click += miGens9_Click;

            miGens1 = new RadioMenuItem();
            miGens1.Text = "1";
            miGens1.Click += miGens9_Click;

            miGens2 = new RadioMenuItem();
            miGens2.Text = "2";
            miGens2.Click += miGens9_Click;

            miGens3 = new RadioMenuItem();
            miGens3.Text = "3";
            miGens3.Click += miGens9_Click;

            miGens4 = new RadioMenuItem();
            miGens4.Text = "4";
            miGens4.Click += miGens9_Click;

            miGens5 = new RadioMenuItem();
            miGens5.Text = "5";
            miGens5.Click += miGens9_Click;

            miGens6 = new RadioMenuItem();
            miGens6.Text = "6";
            miGens6.Click += miGens9_Click;

            miGens7 = new RadioMenuItem();
            miGens7.Text = "7";
            miGens7.Click += miGens9_Click;

            miGens8 = new RadioMenuItem();
            miGens8.Text = "8";
            miGens8.Click += miGens9_Click;

            miGens9 = new RadioMenuItem();
            miGens9.Text = "9";
            miGens9.Click += miGens9_Click;

            MenuGens = new ContextMenu();
            MenuGens.Items.AddRange(new MenuItem[] {
                                        miGensInf,
                                        miGens1,
                                        miGens2,
                                        miGens3,
                                        miGens4,
                                        miGens5,
                                        miGens6,
                                        miGens7,
                                        miGens8,
                                        miGens9});

            miModeBoth = new RadioMenuItem();
            miModeBoth.Text = "miModeBoth";
            miModeBoth.Click += miModeItem_Click;

            miModeAncestors = new RadioMenuItem();
            miModeAncestors.Text = "miModeAncestors";
            miModeAncestors.Click += miModeItem_Click;

            miModeDescendants = new RadioMenuItem();
            miModeDescendants.Text = "miModeDescendants";
            miModeDescendants.Click += miModeItem_Click;

            miTraceSelected = new CheckMenuItem();
            miTraceSelected.Text = "miTraceSelected";
            miTraceSelected.Click += miTraceSelected_Click;

            miTraceKinships = new CheckMenuItem();
            miTraceKinships.Text = "miTraceKinships";
            miTraceKinships.Click += miTraceKinships_Click;

            miCertaintyIndex = new CheckMenuItem();
            miCertaintyIndex.Text = "miCertaintyIndex";
            miCertaintyIndex.Click += miCertaintyIndex_Click;

            miFillColor = new ButtonMenuItem();
            miFillColor.Text = "miFillColor";
            miFillColor.Click += miFillColor_Click;

            miFillImage = new ButtonMenuItem();
            miFillImage.Text = "miFillImage";
            miFillImage.Click += miFillImage_Click;

            MenuModes = new ContextMenu();
            MenuModes.Items.AddRange(new MenuItem[] {
                                         miModeBoth,
                                         miModeAncestors,
                                         miModeDescendants,
                                         new SeparatorMenuItem(),
                                         miTraceSelected,
                                         miTraceKinships,
                                         miCertaintyIndex,
                                         new SeparatorMenuItem(),
                                         miFillColor,
                                         miFillImage,
                                         new SeparatorMenuItem()});

            miEdit = new ButtonMenuItem();
            miEdit.Text = "miEdit";
            miEdit.Click += miEdit_Click;

            miFatherAdd = new ButtonMenuItem();
            miFatherAdd.Text = "miFatherAdd";
            miFatherAdd.Click += miFatherAdd_Click;

            miMotherAdd = new ButtonMenuItem();
            miMotherAdd.Text = "miMotherAdd";
            miMotherAdd.Click += miMotherAdd_Click;

            miFamilyAdd = new ButtonMenuItem();
            miFamilyAdd.Text = "miFamilyAdd";
            miFamilyAdd.Click += miFamilyAdd_Click;

            miSpouseAdd = new ButtonMenuItem();
            miSpouseAdd.Text = "miSpouseAdd";
            miSpouseAdd.Click += miSpouseAdd_Click;

            miSonAdd = new ButtonMenuItem();
            miSonAdd.Text = "miSonAdd";
            miSonAdd.Click += miSonAdd_Click;

            miDaughterAdd = new ButtonMenuItem();
            miDaughterAdd.Text = "miDaughterAdd";
            miDaughterAdd.Click += miDaughterAdd_Click;

            miDelete = new ButtonMenuItem();
            miDelete.Text = "miDelete";
            miDelete.Click += miDelete_Click;

            miRebuildTree = new ButtonMenuItem();
            miRebuildTree.Text = "miRebuildTree";
            miRebuildTree.Click += miRebuildTree_Click;

            miRebuildKinships = new ButtonMenuItem();
            miRebuildKinships.Text = "miRebuildKinships";
            miRebuildKinships.Click += miRebuildKinships_Click;

            MenuPerson = new ContextMenu();
            MenuPerson.Items.AddRange(new MenuItem[] {
                                          miEdit,
                                          new SeparatorMenuItem(),
                                          miFatherAdd,
                                          miMotherAdd,
                                          miFamilyAdd,
                                          miSpouseAdd,
                                          miSonAdd,
                                          miDaughterAdd,
                                          new SeparatorMenuItem(),
                                          miDelete,
                                          new SeparatorMenuItem(),
                                          miRebuildTree,
                                          miRebuildKinships});
            MenuPerson.Opening += MenuPerson_Opening;


            ShowInTaskbar = true;
            Title = "TreeChartWin";
            ToolBar = ToolBar1;
            KeyDown += TreeChartWin_KeyDown;

            UIHelper.SetPredefProperties(this, 820, 450);
            ResumeLayout();
        }
    }
}
