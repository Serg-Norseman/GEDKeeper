using Eto.Forms;
using GKUI.Components;

namespace GKUI.Forms
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
        private ButtonMenuItem miFillColor;
        private ButtonMenuItem miFillImage;
        private ButtonMenuItem miFatherAdd;
        private ButtonMenuItem miMotherAdd;
        private ButtonMenuItem miSelectColor;
        private ButtonMenuItem miGoToRecord;
        private ButtonMenuItem miGoToPrimaryBranch;

        private ButtonToolItem tbDocPrint;
        private ButtonToolItem tbDocPreview;
        private ButtonToolItem tbFilter;
        private ButtonToolItem tbPrev;
        private ButtonToolItem tbNext;
        private ButtonToolItem tbOptions;

        private ButtonToolItem tbGensCommon;
        private ContextMenu MenuGensCommon;
        private ButtonToolItem tbGensAncestors;
        private ContextMenu MenuGensAncestors;
        private ButtonToolItem tbGensDescendants;
        private ContextMenu MenuGensDescendants;
        private ButtonToolItem tbBorders;
        private ContextMenu MenuBorders;

        private void InitializeComponent()
        {
            SuspendLayout();

            tbImageSave = new ButtonToolItem();
            tbImageSave.Click += tbImageSave_Click;

            tbGensCommon = new ButtonToolItem();
            //tbGensCommon.DropDown = MenuGensCommon;
            tbGensCommon.Text = "tbGensCommon";
            tbGensCommon.Click  += (sender, e) => MenuGensCommon.Show(this);

            tbGensAncestors = new ButtonToolItem();
            //tbGensAncestors.DropDown = MenuGensAncestors;
            tbGensAncestors.Text = "tbGensAncestors";
            tbGensAncestors.Click  += (sender, e) => MenuGensAncestors.Show(this);

            tbGensDescendants = new ButtonToolItem();
            //tbGensDescendants.DropDown = MenuGensDescendants;
            tbGensDescendants.Text = "tbGensDescendants";
            tbGensDescendants.Click  += (sender, e) => MenuGensDescendants.Show(this);

            tbModes = new ButtonToolItem();
            //tbModes.DropDown = MenuModes;
            tbModes.Click  += (sender, e) => MenuModes.Show(this);

            tbFilter = new ButtonToolItem();
            tbFilter.Click += ToolBar1_ButtonClick;

            tbPrev = new ButtonToolItem();
            tbPrev.Enabled = false;
            tbPrev.Click += ToolBar1_ButtonClick;

            tbNext = new ButtonToolItem();
            tbNext.Enabled = false;
            tbNext.Click += ToolBar1_ButtonClick;

            tbDocPreview = new ButtonToolItem();
            tbDocPreview.Click += tbDocPreview_Click;

            tbDocPrint = new ButtonToolItem();
            tbDocPrint.Click += tbDocPrint_Click;

            tbOptions = new ButtonToolItem();
            tbOptions.Click += tbOptions_Click;

            tbBorders = new ButtonToolItem();
            tbBorders.Click += (sender, e) => MenuBorders.Show(this);

            ToolBar1 = new ToolBar();
            ToolBar1.TextAlign = ToolBarTextAlign.Right;
            ToolBar1.Items.AddRange(new ToolItem[] {
                                        tbImageSave,
                                        new SeparatorToolItem(),
                                        tbGensCommon,
                                        tbGensAncestors,
                                        tbGensDescendants,
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
                                        tbOptions,
                                        new SeparatorToolItem(),
                                        tbBorders});

            MenuGensCommon = new ContextMenu();
            MenuGensAncestors = new ContextMenu();
            MenuGensDescendants = new ContextMenu();
            MenuBorders = new ContextMenu();

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

            miSelectColor = new ButtonMenuItem();
            miSelectColor.Text = "miSelectColor";
            miSelectColor.Click += miSelectColor_Click;

            miGoToRecord = new ButtonMenuItem();
            miGoToRecord.Text = "miGoToRecord";
            miGoToRecord.Click += miGoToRecord_Click;

            miGoToPrimaryBranch = new ButtonMenuItem();
            miGoToPrimaryBranch.Text = "miGoToPrimaryBranch";
            miGoToPrimaryBranch.Click += miGoToPrimaryBranch_Click;

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
                                          miGoToRecord,
                                          miGoToPrimaryBranch,
                                          new SeparatorMenuItem(),
                                          miRebuildTree,
                                          miRebuildKinships,
                                          new SeparatorMenuItem(),
                                          miSelectColor});
            MenuPerson.Opening += MenuPerson_Opening;


            ShowInTaskbar = true;
            Title = "TreeChartWin";
            ToolBar = ToolBar1;
            KeyDown += TreeChartWin_KeyDown;

            UIHelper.SetPredefProperties(this, 820, 450, true, true);
            ResumeLayout();
        }
    }
}
