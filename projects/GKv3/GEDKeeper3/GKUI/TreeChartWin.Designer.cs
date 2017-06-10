using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI
{
    partial class TreeChartWin
    {
        private ToolBar ToolBar1;
        private ButtonToolItem tbImageSave;
        private ContextMenu MenuPerson;
        private ButtonMenuItem miEdit;
        private SeparatorMenuItem N1;
        private ButtonMenuItem miSpouseAdd;
        private ButtonMenuItem miSonAdd;
        private ButtonMenuItem miDaughterAdd;
        private ButtonMenuItem miFamilyAdd;
        private SeparatorMenuItem N2;
        private ButtonMenuItem miDelete;
        private SeparatorMenuItem N3;
        private ButtonMenuItem miRebuildKinships;
        private ButtonToolItem tbModes; // FIXME: GKv3 DevRestriction
        private ContextMenu MenuModes;
        private RadioMenuItem miModeBoth;
        private RadioMenuItem miModeAncestors;
        private RadioMenuItem miModeDescendants;
        private SeparatorMenuItem N7;
        private CheckMenuItem miTraceSelected;
        private CheckMenuItem miTraceKinships;
        private CheckMenuItem miCertaintyIndex;
        private ButtonMenuItem miRebuildTree;
        private ButtonToolItem tbGens; // FIXME: GKv3 DevRestriction
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
        private SeparatorMenuItem N8;
        private ButtonMenuItem miFillColor;
        private ButtonMenuItem miFillImage;
        private SeparatorMenuItem N9;

        private ColorDialog colorDialog1;
        private SeparatorToolItem tbs2;
        private SeparatorToolItem tbs1;
        private ButtonMenuItem miFatherAdd;
        private ButtonMenuItem miMotherAdd;
        private SeparatorToolItem SeparatorToolItem2;
        private ButtonToolItem tbDocPrint;
        private ButtonToolItem tbDocPreview;
        private SeparatorToolItem SeparatorToolItem1;
        private ButtonToolItem tbFilter;
        private ButtonToolItem tbPrev;
        private ButtonToolItem tbNext;

        private void InitializeComponent()
        {
            ToolBar1 = new ToolBar();
            tbImageSave = new ButtonToolItem();
            tbs1 = new SeparatorToolItem();
            tbGens = new ButtonToolItem();
            MenuGens = new ContextMenu();
            miGensInf = new RadioMenuItem();
            miGens1 = new RadioMenuItem();
            miGens2 = new RadioMenuItem();
            miGens3 = new RadioMenuItem();
            miGens4 = new RadioMenuItem();
            miGens5 = new RadioMenuItem();
            miGens6 = new RadioMenuItem();
            miGens7 = new RadioMenuItem();
            miGens8 = new RadioMenuItem();
            miGens9 = new RadioMenuItem();
            tbs2 = new SeparatorToolItem();
            tbModes = new ButtonToolItem();
            MenuModes = new ContextMenu();
            miModeBoth = new RadioMenuItem();
            miModeAncestors = new RadioMenuItem();
            miModeDescendants = new RadioMenuItem();
            N7 = new SeparatorMenuItem();
            miTraceSelected = new CheckMenuItem();
            miTraceKinships = new CheckMenuItem();
            miCertaintyIndex = new CheckMenuItem();
            N8 = new SeparatorMenuItem();
            miFillColor = new ButtonMenuItem();
            miFillImage = new ButtonMenuItem();
            N9 = new SeparatorMenuItem();
            SeparatorToolItem1 = new SeparatorToolItem();
            tbFilter = new ButtonToolItem();
            tbPrev = new ButtonToolItem();
            tbNext = new ButtonToolItem();
            tbDocPreview = new ButtonToolItem();
            tbDocPrint = new ButtonToolItem();
            MenuPerson = new ContextMenu();
            miEdit = new ButtonMenuItem();
            N1 = new SeparatorMenuItem();
            miFatherAdd = new ButtonMenuItem();
            miMotherAdd = new ButtonMenuItem();
            miFamilyAdd = new ButtonMenuItem();
            miSpouseAdd = new ButtonMenuItem();
            miSonAdd = new ButtonMenuItem();
            miDaughterAdd = new ButtonMenuItem();
            N2 = new SeparatorMenuItem();
            miDelete = new ButtonMenuItem();
            N3 = new SeparatorMenuItem();
            miRebuildTree = new ButtonMenuItem();
            miRebuildKinships = new ButtonMenuItem();
            colorDialog1 = new ColorDialog();
            SeparatorToolItem2 = new SeparatorToolItem();
            SuspendLayout();

            ToolBar1.Items.AddRange(new ToolItem[] {
                                        tbImageSave,
                                        tbs1,
                                        tbGens,
                                        tbs2,
                                        tbModes,
                                        SeparatorToolItem1,
                                        tbFilter,
                                        tbPrev,
                                        tbNext,
                                        SeparatorToolItem2,
                                        tbDocPreview,
                                        tbDocPrint});

            tbImageSave.Click += tbImageSave_Click;

            //tbGens.DropDown = MenuGens;
            tbGens.Text = "tbGens";
            tbGens.Click  += (sender, e) => MenuGens.Show(this);

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

            miGensInf.Checked = true;
            miGensInf.Text = "Inf";
            miGensInf.Click += miGens9_Click;

            miGens1.Text = "1";
            miGens1.Click += miGens9_Click;

            miGens2.Text = "2";
            miGens2.Click += miGens9_Click;

            miGens3.Text = "3";
            miGens3.Click += miGens9_Click;

            miGens4.Text = "4";
            miGens4.Click += miGens9_Click;

            miGens5.Text = "5";
            miGens5.Click += miGens9_Click;

            miGens6.Text = "6";
            miGens6.Click += miGens9_Click;

            miGens7.Text = "7";
            miGens7.Click += miGens9_Click;

            miGens8.Text = "8";
            miGens8.Click += miGens9_Click;

            miGens9.Text = "9";
            miGens9.Click += miGens9_Click;

            //tbModes.DropDown = MenuModes;
            tbModes.Click  += (sender, e) => MenuModes.Show(this);

            MenuModes.Items.AddRange(new MenuItem[] {
                                         miModeBoth,
                                         miModeAncestors,
                                         miModeDescendants,
                                         N7,
                                         miTraceSelected,
                                         miTraceKinships,
                                         miCertaintyIndex,
                                         N8,
                                         miFillColor,
                                         miFillImage,
                                         N9});

            miModeBoth.Text = "miModeBoth";
            miModeBoth.Click += miModeItem_Click;

            miModeAncestors.Text = "miModeAncestors";
            miModeAncestors.Click += miModeItem_Click;

            miModeDescendants.Text = "miModeDescendants";
            miModeDescendants.Click += miModeItem_Click;

            miTraceSelected.Text = "miTraceSelected";
            miTraceSelected.Click += miTraceSelected_Click;

            miTraceKinships.Text = "miTraceKinships";
            miTraceKinships.Click += miTraceKinships_Click;

            miCertaintyIndex.Text = "miCertaintyIndex";
            miCertaintyIndex.Click += miCertaintyIndex_Click;

            miFillColor.Text = "miFillColor";
            miFillColor.Click += miFillColor_Click;

            miFillImage.Text = "miFillImage";
            miFillImage.Click += miFillImage_Click;

            tbFilter.Click += ToolBar1_ButtonClick;

            tbPrev.Enabled = false;
            tbPrev.Click += ToolBar1_ButtonClick;

            tbNext.Enabled = false;
            tbNext.Click += ToolBar1_ButtonClick;

            tbDocPreview.Click += tbDocPreview_Click;

            tbDocPrint.Click += tbDocPrint_Click;

            MenuPerson.Items.AddRange(new MenuItem[] {
                                          miEdit,
                                          N1,
                                          miFatherAdd,
                                          miMotherAdd,
                                          miFamilyAdd,
                                          miSpouseAdd,
                                          miSonAdd,
                                          miDaughterAdd,
                                          N2,
                                          miDelete,
                                          N3,
                                          miRebuildTree,
                                          miRebuildKinships});
            MenuPerson.Opening += MenuPerson_Opening;

            miEdit.Text = "miEdit";
            miEdit.Click += miEdit_Click;

            miFatherAdd.Text = "miFatherAdd";
            miFatherAdd.Click += miFatherAdd_Click;

            miMotherAdd.Text = "miMotherAdd";
            miMotherAdd.Click += miMotherAdd_Click;

            miFamilyAdd.Text = "miFamilyAdd";
            miFamilyAdd.Click += miFamilyAdd_Click;

            miSpouseAdd.Text = "miSpouseAdd";
            miSpouseAdd.Click += miSpouseAdd_Click;

            miSonAdd.Text = "miSonAdd";
            miSonAdd.Click += miSonAdd_Click;

            miDaughterAdd.Text = "miDaughterAdd";
            miDaughterAdd.Click += miDaughterAdd_Click;

            miDelete.Text = "miDelete";
            miDelete.Click += miDelete_Click;

            miRebuildTree.Text = "miRebuildTree";
            miRebuildTree.Click += miRebuildTree_Click;

            miRebuildKinships.Text = "miRebuildKinships";
            miRebuildKinships.Click += miRebuildKinships_Click;

            ClientSize = new Size(822, 452);
            //Font = new Font("Tahoma", 8.25F, FontStyle.None);
            Title = "TreeChartWin";
            KeyDown += TreeChartWin_KeyDown;
            ResumeLayout();
        }
    }
}
