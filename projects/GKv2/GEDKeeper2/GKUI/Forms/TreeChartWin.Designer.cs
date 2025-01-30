namespace GKUI.Forms
{
    partial class TreeChartWin
    {
        private System.Windows.Forms.ToolStrip ToolBar1;
        private System.Windows.Forms.ToolStripButton tbImageSave;
        private System.Windows.Forms.ContextMenuStrip MenuPerson;
        private System.Windows.Forms.ToolStripMenuItem miEdit;
        private System.Windows.Forms.ToolStripSeparator N1;
        private System.Windows.Forms.ToolStripMenuItem miSpouseAdd;
        private System.Windows.Forms.ToolStripMenuItem miSonAdd;
        private System.Windows.Forms.ToolStripMenuItem miDaughterAdd;
        private System.Windows.Forms.ToolStripMenuItem miFamilyAdd;
        private System.Windows.Forms.ToolStripSeparator N2;
        private System.Windows.Forms.ToolStripMenuItem miDelete;
        private System.Windows.Forms.ToolStripSeparator N3;
        private System.Windows.Forms.ToolStripMenuItem miRebuildKinships;
        private System.Windows.Forms.ToolStripDropDownButton tbModes;
        private System.Windows.Forms.ContextMenuStrip MenuModes;
        private System.Windows.Forms.ToolStripMenuItem miModeBoth;
        private System.Windows.Forms.ToolStripMenuItem miModeAncestors;
        private System.Windows.Forms.ToolStripMenuItem miModeDescendants;
        private System.Windows.Forms.ToolStripSeparator N7;
        private System.Windows.Forms.ToolStripMenuItem miTraceSelected;
        private System.Windows.Forms.ToolStripMenuItem miTraceKinships;
        private System.Windows.Forms.ToolStripMenuItem miCertaintyIndex;
        private System.Windows.Forms.ToolStripMenuItem miXRefVisible;
        private System.Windows.Forms.ToolStripMenuItem miTrackSelectedLines;
        private System.Windows.Forms.ToolStripMenuItem miTrackMatchedSources;
        private System.Windows.Forms.ToolStripMenuItem miRebuildTree;
        private System.Windows.Forms.ToolStripSeparator N8;
        private System.Windows.Forms.ToolStripMenuItem miFillColor;
        private System.Windows.Forms.ToolStripMenuItem miFillImage;
        private System.Windows.Forms.ToolStripSeparator N9;
        private System.ComponentModel.IContainer components;
        private System.Windows.Forms.ToolStripSeparator tbs2;
        private System.Windows.Forms.ToolStripSeparator tbs1;
        private System.Windows.Forms.ToolStripMenuItem miFatherAdd;
        private System.Windows.Forms.ToolStripMenuItem miMotherAdd;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
        private System.Windows.Forms.ToolStripButton tbDocPrint;
        private System.Windows.Forms.ToolStripButton tbDocPreview;
        private System.Windows.Forms.ToolStripButton tbOptions;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
        private System.Windows.Forms.ToolStripButton tbFilter;
        private System.Windows.Forms.ToolStripButton tbPrev;
        private System.Windows.Forms.ToolStripButton tbNext;
        private System.Windows.Forms.ToolStripSeparator N10;
        private System.Windows.Forms.ToolStripMenuItem miSelectColor;
        private System.Windows.Forms.ToolStripSeparator N11;
        private System.Windows.Forms.ToolStripMenuItem miGoToRecord;
        private System.Windows.Forms.ToolStripDropDownButton tbGensCommon;
        private System.Windows.Forms.ContextMenuStrip MenuGensCommon;
        private System.Windows.Forms.ToolStripDropDownButton tbGensAncestors;
        private System.Windows.Forms.ContextMenuStrip MenuGensAncestors;
        private System.Windows.Forms.ToolStripDropDownButton tbGensDescendants;
        private System.Windows.Forms.ContextMenuStrip MenuGensDescendants;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator3;
        private System.Windows.Forms.ToolStripDropDownButton tbBorders;
        private System.Windows.Forms.ContextMenuStrip MenuBorders;
        private System.Windows.Forms.ToolStripMenuItem miGoToPrimaryBranch;
        private System.Windows.Forms.ToolStripMenuItem miOpenInNewWindow;
        private System.Windows.Forms.ToolStripMenuItem miMergeDuplicates;
        private System.Windows.Forms.ToolStripSeparator N15;
        private System.Windows.Forms.ToolStripMenuItem miHideDescSpouses;
        private System.Windows.Forms.ToolStripMenuItem miParentAges;
        private System.Windows.Forms.ToolStripMenuItem miMaps;
        private System.Windows.Forms.ToolStripSeparator N16;
        private System.Windows.Forms.ToolStripMenuItem miMapAncestors;
        private System.Windows.Forms.ToolStripMenuItem miMapDescendants;
        private System.Windows.Forms.ToolStripMenuItem miMapAll;

        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.ToolBar1 = new System.Windows.Forms.ToolStrip();
            this.tbImageSave = new System.Windows.Forms.ToolStripButton();
            this.tbs1 = new System.Windows.Forms.ToolStripSeparator();
            this.tbGensCommon = new System.Windows.Forms.ToolStripDropDownButton();
            this.MenuGensCommon = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.tbGensAncestors = new System.Windows.Forms.ToolStripDropDownButton();
            this.MenuGensAncestors = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.tbGensDescendants = new System.Windows.Forms.ToolStripDropDownButton();
            this.MenuGensDescendants = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.tbs2 = new System.Windows.Forms.ToolStripSeparator();
            this.tbModes = new System.Windows.Forms.ToolStripDropDownButton();
            this.MenuModes = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.miModeBoth = new System.Windows.Forms.ToolStripMenuItem();
            this.miModeAncestors = new System.Windows.Forms.ToolStripMenuItem();
            this.miModeDescendants = new System.Windows.Forms.ToolStripMenuItem();
            this.N7 = new System.Windows.Forms.ToolStripSeparator();
            this.miTraceSelected = new System.Windows.Forms.ToolStripMenuItem();
            this.miTraceKinships = new System.Windows.Forms.ToolStripMenuItem();
            this.miCertaintyIndex = new System.Windows.Forms.ToolStripMenuItem();
            this.miXRefVisible = new System.Windows.Forms.ToolStripMenuItem();
            this.miTrackSelectedLines = new System.Windows.Forms.ToolStripMenuItem();
            this.miTrackMatchedSources = new System.Windows.Forms.ToolStripMenuItem();
            this.N8 = new System.Windows.Forms.ToolStripSeparator();
            this.miFillColor = new System.Windows.Forms.ToolStripMenuItem();
            this.miFillImage = new System.Windows.Forms.ToolStripMenuItem();
            this.N9 = new System.Windows.Forms.ToolStripSeparator();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.tbFilter = new System.Windows.Forms.ToolStripButton();
            this.tbPrev = new System.Windows.Forms.ToolStripButton();
            this.tbNext = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
            this.tbDocPreview = new System.Windows.Forms.ToolStripButton();
            this.tbDocPrint = new System.Windows.Forms.ToolStripButton();
            this.tbOptions = new System.Windows.Forms.ToolStripButton();
            this.MenuPerson = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.miEdit = new System.Windows.Forms.ToolStripMenuItem();
            this.N1 = new System.Windows.Forms.ToolStripSeparator();
            this.miFatherAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.miMotherAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.miFamilyAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.miSpouseAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.miSonAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.miDaughterAdd = new System.Windows.Forms.ToolStripMenuItem();
            this.N2 = new System.Windows.Forms.ToolStripSeparator();
            this.miDelete = new System.Windows.Forms.ToolStripMenuItem();
            this.N11 = new System.Windows.Forms.ToolStripSeparator();
            this.miGoToRecord = new System.Windows.Forms.ToolStripMenuItem();
            this.N3 = new System.Windows.Forms.ToolStripSeparator();
            this.miRebuildTree = new System.Windows.Forms.ToolStripMenuItem();
            this.miRebuildKinships = new System.Windows.Forms.ToolStripMenuItem();
            this.N10 = new System.Windows.Forms.ToolStripSeparator();
            this.miSelectColor = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripSeparator3 = new System.Windows.Forms.ToolStripSeparator();
            this.tbBorders = new System.Windows.Forms.ToolStripDropDownButton();
            this.MenuBorders = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.miGoToPrimaryBranch = new System.Windows.Forms.ToolStripMenuItem();
            this.miOpenInNewWindow = new System.Windows.Forms.ToolStripMenuItem();
            this.miMergeDuplicates = new System.Windows.Forms.ToolStripMenuItem();
            this.N15 = new System.Windows.Forms.ToolStripSeparator();
            this.miHideDescSpouses = new System.Windows.Forms.ToolStripMenuItem();
            this.miParentAges = new System.Windows.Forms.ToolStripMenuItem();
            this.miMaps = new System.Windows.Forms.ToolStripMenuItem();
            this.N16 = new System.Windows.Forms.ToolStripSeparator();
            this.miMapAncestors = new System.Windows.Forms.ToolStripMenuItem();
            this.miMapDescendants = new System.Windows.Forms.ToolStripMenuItem();
            this.miMapAll = new System.Windows.Forms.ToolStripMenuItem();
            this.ToolBar1.SuspendLayout();
            this.MenuModes.SuspendLayout();
            this.MenuPerson.SuspendLayout();
            this.SuspendLayout();
            // 
            // ToolBar1
            // 
            this.ToolBar1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden;
            this.ToolBar1.ImageScalingSize = new System.Drawing.Size(20, 20);
            this.ToolBar1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.tbImageSave,
            this.tbs1,
            this.tbGensCommon,
            this.tbGensAncestors,
            this.tbGensDescendants,
            this.tbs2,
            this.tbModes,
            this.toolStripSeparator1,
            this.tbFilter,
            this.tbPrev,
            this.tbNext,
            this.toolStripSeparator2,
            this.tbDocPreview,
            this.tbDocPrint,
            this.tbOptions,
            this.toolStripSeparator3,
            this.tbBorders});
            this.ToolBar1.Location = new System.Drawing.Point(0, 0);
            this.ToolBar1.Name = "ToolBar1";
            this.ToolBar1.Size = new System.Drawing.Size(771, 25);
            this.ToolBar1.TabIndex = 0;
            // 
            // tbImageSave
            // 
            this.tbImageSave.Name = "tbImageSave";
            this.tbImageSave.Size = new System.Drawing.Size(23, 22);
            this.tbImageSave.Click += new System.EventHandler(this.tbImageSave_Click);
            // 
            // tbs1
            // 
            this.tbs1.Name = "tbs1";
            this.tbs1.Size = new System.Drawing.Size(6, 25);
            // 
            // tbGensCommon
            // 
            this.tbGensCommon.DropDown = this.MenuGensCommon;
            this.tbGensCommon.Name = "tbGensCommon";
            this.tbGensCommon.Size = new System.Drawing.Size(108, 22);
            this.tbGensCommon.Text = "tbGensCommon";
            // 
            // MenuGensCommon
            // 
            this.MenuGensCommon.Name = "MenuGensCommon";
            this.MenuGensCommon.OwnerItem = this.tbGensCommon;
            this.MenuGensCommon.Size = new System.Drawing.Size(61, 4);
            // 
            // tbGensAncestors
            // 
            this.tbGensAncestors.DropDown = this.MenuGensAncestors;
            this.tbGensAncestors.Name = "tbGensAncestors";
            this.tbGensAncestors.Size = new System.Drawing.Size(109, 22);
            this.tbGensAncestors.Text = "tbGensAncestors";
            // 
            // MenuGensAncestors
            // 
            this.MenuGensAncestors.Name = "MenuGensAncestors";
            this.MenuGensAncestors.Size = new System.Drawing.Size(61, 4);
            // 
            // tbGensDescendants
            // 
            this.tbGensDescendants.DropDown = this.MenuGensDescendants;
            this.tbGensDescendants.Name = "tbGensDescendants";
            this.tbGensDescendants.Size = new System.Drawing.Size(124, 22);
            this.tbGensDescendants.Text = "tbGensDescendants";
            // 
            // MenuGensDescendants
            // 
            this.MenuGensDescendants.Name = "MenuGensDescendants";
            this.MenuGensDescendants.Size = new System.Drawing.Size(61, 4);
            // 
            // tbs2
            // 
            this.tbs2.Name = "tbs2";
            this.tbs2.Size = new System.Drawing.Size(6, 25);
            // 
            // tbModes
            // 
            this.tbModes.DropDown = this.MenuModes;
            this.tbModes.Name = "tbModes";
            this.tbModes.Size = new System.Drawing.Size(13, 22);
            // 
            // MenuModes
            // 
            this.MenuModes.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.miModeBoth,
            this.miModeAncestors,
            this.miModeDescendants,
            this.N15,
            this.miHideDescSpouses,
            this.N7,
            this.miTraceSelected,
            this.miTraceKinships,
            this.miCertaintyIndex,
            this.miXRefVisible,
            this.miTrackSelectedLines,
            this.miTrackMatchedSources,
            this.miParentAges,
            this.N8,
            this.miFillColor,
            this.miFillImage,
            this.N9});
            this.MenuModes.Name = "MenuModes";
            this.MenuModes.OwnerItem = this.tbModes;
            this.MenuModes.Size = new System.Drawing.Size(187, 198);
            // 
            // miModeBoth
            // 
            this.miModeBoth.Name = "miModeBoth";
            this.miModeBoth.Size = new System.Drawing.Size(186, 22);
            this.miModeBoth.Text = "miModeBoth";
            this.miModeBoth.Click += new System.EventHandler(this.miModeItem_Click);
            // 
            // miModeAncestors
            // 
            this.miModeAncestors.Name = "miModeAncestors";
            this.miModeAncestors.Size = new System.Drawing.Size(186, 22);
            this.miModeAncestors.Text = "miModeAncestors";
            this.miModeAncestors.Click += new System.EventHandler(this.miModeItem_Click);
            // 
            // miModeDescendants
            // 
            this.miModeDescendants.Name = "miModeDescendants";
            this.miModeDescendants.Size = new System.Drawing.Size(186, 22);
            this.miModeDescendants.Text = "miModeDescendants";
            this.miModeDescendants.Click += new System.EventHandler(this.miModeItem_Click);
            // 
            // N7
            // 
            this.N7.Name = "N7";
            this.N7.Size = new System.Drawing.Size(183, 6);
            // 
            // N15
            // 
            this.N15.Name = "N15";
            this.N15.Size = new System.Drawing.Size(183, 6);
            // 
            // miHideDescSpouses
            // 
            this.miHideDescSpouses.Name = "miHideDescSpouses";
            this.miHideDescSpouses.Size = new System.Drawing.Size(186, 22);
            this.miHideDescSpouses.Text = "miHideDescSpouses";
            this.miHideDescSpouses.Click += new System.EventHandler(this.miHideDescSpouses_Click);
            // 
            // miTraceSelected
            // 
            this.miTraceSelected.Name = "miTraceSelected";
            this.miTraceSelected.Size = new System.Drawing.Size(186, 22);
            this.miTraceSelected.Text = "miTraceSelected";
            this.miTraceSelected.Click += new System.EventHandler(this.miTraceSelected_Click);
            // 
            // miTraceKinships
            // 
            this.miTraceKinships.Name = "miTraceKinships";
            this.miTraceKinships.Size = new System.Drawing.Size(186, 22);
            this.miTraceKinships.Text = "miTraceKinships";
            this.miTraceKinships.Click += new System.EventHandler(this.miTraceKinships_Click);
            // 
            // miCertaintyIndex
            // 
            this.miCertaintyIndex.Name = "miCertaintyIndex";
            this.miCertaintyIndex.Size = new System.Drawing.Size(186, 22);
            this.miCertaintyIndex.Text = "miCertaintyIndex";
            this.miCertaintyIndex.Click += new System.EventHandler(this.miCertaintyIndex_Click);
            // 
            // miXRefVisible
            // 
            this.miXRefVisible.Name = "miXRefVisible";
            this.miXRefVisible.Size = new System.Drawing.Size(186, 22);
            this.miXRefVisible.Text = "miXRefVisible";
            this.miXRefVisible.Click += new System.EventHandler(this.miXRefVisible_Click);
            // 
            // miTrackSelectedLines
            // 
            this.miTrackSelectedLines.Name = "miTrackSelectedLines";
            this.miTrackSelectedLines.Size = new System.Drawing.Size(186, 22);
            this.miTrackSelectedLines.Text = "miTrackSelectedLines";
            this.miTrackSelectedLines.Click += new System.EventHandler(this.miTrackSelectedLines_Click);
            // 
            // miTrackMatchedSources
            // 
            this.miTrackMatchedSources.Name = "miTrackMatchedSources";
            this.miTrackMatchedSources.Size = new System.Drawing.Size(186, 22);
            this.miTrackMatchedSources.Text = "miTrackMatchedSources";
            this.miTrackMatchedSources.Click += new System.EventHandler(this.miTrackMatchedSources_Click);
            // 
            // miParentAges
            // 
            this.miParentAges.Name = "miParentAges";
            this.miParentAges.Size = new System.Drawing.Size(186, 22);
            this.miParentAges.Text = "miParentAges";
            this.miParentAges.Click += new System.EventHandler(this.miParentAges_Click);
            // 
            // N8
            // 
            this.N8.Name = "N8";
            this.N8.Size = new System.Drawing.Size(183, 6);
            // 
            // miFillColor
            // 
            this.miFillColor.Name = "miFillColor";
            this.miFillColor.Size = new System.Drawing.Size(186, 22);
            this.miFillColor.Text = "miFillColor";
            this.miFillColor.Click += new System.EventHandler(this.miFillColor_Click);
            // 
            // miFillImage
            // 
            this.miFillImage.Name = "miFillImage";
            this.miFillImage.Size = new System.Drawing.Size(186, 22);
            this.miFillImage.Text = "miFillImage";
            this.miFillImage.Click += new System.EventHandler(this.miFillImage_Click);
            // 
            // N9
            // 
            this.N9.Name = "N9";
            this.N9.Size = new System.Drawing.Size(183, 6);
            this.N9.Visible = false;
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 25);
            // 
            // tbFilter
            // 
            this.tbFilter.ImageTransparentColor = System.Drawing.Color.White;
            this.tbFilter.Name = "tbFilter";
            this.tbFilter.Size = new System.Drawing.Size(23, 22);
            this.tbFilter.Click += new System.EventHandler(this.ToolBar1_ButtonClick);
            // 
            // tbPrev
            // 
            this.tbPrev.Enabled = false;
            this.tbPrev.Name = "tbPrev";
            this.tbPrev.Size = new System.Drawing.Size(23, 22);
            this.tbPrev.Click += new System.EventHandler(this.ToolBar1_ButtonClick);
            // 
            // tbNext
            // 
            this.tbNext.Enabled = false;
            this.tbNext.Name = "tbNext";
            this.tbNext.Size = new System.Drawing.Size(23, 22);
            this.tbNext.Click += new System.EventHandler(this.ToolBar1_ButtonClick);
            // 
            // toolStripSeparator2
            // 
            this.toolStripSeparator2.Name = "toolStripSeparator2";
            this.toolStripSeparator2.Size = new System.Drawing.Size(6, 25);
            // 
            // tbDocPreview
            // 
            this.tbDocPreview.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.tbDocPreview.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbDocPreview.Name = "tbDocPreview";
            this.tbDocPreview.Size = new System.Drawing.Size(23, 22);
            this.tbDocPreview.Text = "toolStripButton1";
            this.tbDocPreview.Click += new System.EventHandler(this.tbDocPreview_Click);
            // 
            // tbDocPrint
            // 
            this.tbDocPrint.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.tbDocPrint.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbDocPrint.Name = "tbDocPrint";
            this.tbDocPrint.Size = new System.Drawing.Size(23, 22);
            this.tbDocPrint.Text = "toolStripButton2";
            this.tbDocPrint.Click += new System.EventHandler(this.tbDocPrint_Click);
            // 
            // tbOptions
            // 
            this.tbOptions.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.tbOptions.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbOptions.Name = "tbOptions";
            this.tbOptions.Size = new System.Drawing.Size(23, 22);
            this.tbOptions.Text = "tbOptions";
            this.tbOptions.Click += new System.EventHandler(this.tbOptions_Click);
            // 
            // MenuPerson
            // 
            this.MenuPerson.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.miEdit,
            this.N1,
            this.miFatherAdd,
            this.miMotherAdd,
            this.miFamilyAdd,
            this.miSpouseAdd,
            this.miSonAdd,
            this.miDaughterAdd,
            this.N2,
            this.miDelete,
            this.N11,
            this.miGoToRecord,
            this.miGoToPrimaryBranch,
            this.miOpenInNewWindow,
            this.miMergeDuplicates,
            this.N3,
            this.miRebuildTree,
            this.miRebuildKinships,
            this.N10,
            this.miMaps,
            this.N16,
            this.miSelectColor});
            this.MenuPerson.Name = "MenuPerson";
            this.MenuPerson.Size = new System.Drawing.Size(173, 298);
            this.MenuPerson.Opening += new System.ComponentModel.CancelEventHandler(this.MenuPerson_Opening);
            // 
            // miEdit
            // 
            this.miEdit.Name = "miEdit";
            this.miEdit.Size = new System.Drawing.Size(172, 22);
            this.miEdit.Text = "miEdit";
            this.miEdit.Click += new System.EventHandler(this.miEdit_Click);
            // 
            // N1
            // 
            this.N1.Name = "N1";
            this.N1.Size = new System.Drawing.Size(169, 6);
            // 
            // miFatherAdd
            // 
            this.miFatherAdd.Name = "miFatherAdd";
            this.miFatherAdd.Size = new System.Drawing.Size(172, 22);
            this.miFatherAdd.Text = "miFatherAdd";
            this.miFatherAdd.Click += new System.EventHandler(this.miFatherAdd_Click);
            // 
            // miMotherAdd
            // 
            this.miMotherAdd.Name = "miMotherAdd";
            this.miMotherAdd.Size = new System.Drawing.Size(172, 22);
            this.miMotherAdd.Text = "miMotherAdd";
            this.miMotherAdd.Click += new System.EventHandler(this.miMotherAdd_Click);
            // 
            // miFamilyAdd
            // 
            this.miFamilyAdd.Name = "miFamilyAdd";
            this.miFamilyAdd.Size = new System.Drawing.Size(172, 22);
            this.miFamilyAdd.Text = "miFamilyAdd";
            this.miFamilyAdd.Click += new System.EventHandler(this.miFamilyAdd_Click);
            // 
            // miSpouseAdd
            // 
            this.miSpouseAdd.Name = "miSpouseAdd";
            this.miSpouseAdd.Size = new System.Drawing.Size(172, 22);
            this.miSpouseAdd.Text = "miSpouseAdd";
            this.miSpouseAdd.Click += new System.EventHandler(this.miSpouseAdd_Click);
            // 
            // miSonAdd
            // 
            this.miSonAdd.Name = "miSonAdd";
            this.miSonAdd.Size = new System.Drawing.Size(172, 22);
            this.miSonAdd.Text = "miSonAdd";
            this.miSonAdd.Click += new System.EventHandler(this.miSonAdd_Click);
            // 
            // miDaughterAdd
            // 
            this.miDaughterAdd.Name = "miDaughterAdd";
            this.miDaughterAdd.Size = new System.Drawing.Size(172, 22);
            this.miDaughterAdd.Text = "miDaughterAdd";
            this.miDaughterAdd.Click += new System.EventHandler(this.miDaughterAdd_Click);
            // 
            // N2
            // 
            this.N2.Name = "N2";
            this.N2.Size = new System.Drawing.Size(169, 6);
            // 
            // miDelete
            // 
            this.miDelete.Name = "miDelete";
            this.miDelete.Size = new System.Drawing.Size(172, 22);
            this.miDelete.Text = "miDelete";
            this.miDelete.Click += new System.EventHandler(this.miDelete_Click);
            // 
            // N11
            // 
            this.N11.Name = "N11";
            this.N11.Size = new System.Drawing.Size(169, 6);
            // 
            // miGoToRecord
            // 
            this.miGoToRecord.Name = "miGoToRecord";
            this.miGoToRecord.Size = new System.Drawing.Size(172, 22);
            this.miGoToRecord.Text = "miGoToRecord";
            this.miGoToRecord.Click += new System.EventHandler(this.miGoToRecord_Click);
            // 
            // miGoToPrimaryBranch
            // 
            this.miGoToPrimaryBranch.Name = "miGoToPrimaryBranch";
            this.miGoToPrimaryBranch.Size = new System.Drawing.Size(172, 22);
            this.miGoToPrimaryBranch.Text = "miGoToPrimaryBranch";
            this.miGoToPrimaryBranch.Click += new System.EventHandler(this.miGoToPrimaryBranch_Click);
            // 
            // miOpenInNewWindow
            // 
            this.miOpenInNewWindow.Name = "miOpenInNewWindow";
            this.miOpenInNewWindow.Size = new System.Drawing.Size(172, 22);
            this.miOpenInNewWindow.Text = "miOpenInNewWindow";
            this.miOpenInNewWindow.Click += new System.EventHandler(this.miOpenInNewWindow_Click);
            // 
            // miMergeDuplicates
            // 
            this.miMergeDuplicates.Name = "miMergeDuplicates";
            this.miMergeDuplicates.Size = new System.Drawing.Size(217, 24);
            this.miMergeDuplicates.Text = "miMergeDuplicates";
            this.miMergeDuplicates.Click += new System.EventHandler(this.miMergeDuplicates_Click);
            // 
            // N3
            // 
            this.N3.Name = "N3";
            this.N3.Size = new System.Drawing.Size(169, 6);
            // 
            // miRebuildTree
            // 
            this.miRebuildTree.Name = "miRebuildTree";
            this.miRebuildTree.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.F6)));
            this.miRebuildTree.Size = new System.Drawing.Size(172, 22);
            this.miRebuildTree.Text = "miRebuildTree";
            this.miRebuildTree.Click += new System.EventHandler(this.miRebuildTree_Click);
            // 
            // miRebuildKinships
            // 
            this.miRebuildKinships.Name = "miRebuildKinships";
            this.miRebuildKinships.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.F7)));
            this.miRebuildKinships.Size = new System.Drawing.Size(172, 22);
            this.miRebuildKinships.Text = "miRebuildKinships";
            this.miRebuildKinships.Click += new System.EventHandler(this.miRebuildKinships_Click);
            // 
            // N10
            // 
            this.N10.Name = "N10";
            this.N10.Size = new System.Drawing.Size(169, 6);
            // 
            // miMaps
            // 
            this.miMaps.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.miMapAncestors,
            this.miMapDescendants,
            this.miMapAll});
            this.miMaps.Name = "miMaps";
            this.miMaps.Size = new System.Drawing.Size(172, 22);
            this.miMaps.Text = "miMaps";
            // 
            // miMapAncestors
            // 
            this.miMapAncestors.Name = "miMapAncestors";
            this.miMapAncestors.Size = new System.Drawing.Size(172, 22);
            this.miMapAncestors.Text = "miMapAncestors";
            this.miMapAncestors.Click += new System.EventHandler(this.miMapAncestors_Click);
            // 
            // miMapDescendants
            // 
            this.miMapDescendants.Name = "miMapDescendants";
            this.miMapDescendants.Size = new System.Drawing.Size(172, 22);
            this.miMapDescendants.Text = "miMapDescendants";
            this.miMapDescendants.Click += new System.EventHandler(this.miMapDescendants_Click);
            // 
            // miMapAll
            // 
            this.miMapAll.Name = "miMapAll";
            this.miMapAll.Size = new System.Drawing.Size(172, 22);
            this.miMapAll.Text = "miMapAll";
            this.miMapAll.Click += new System.EventHandler(this.miMapAll_Click);
            // 
            // N16
            // 
            this.N16.Name = "N16";
            this.N16.Size = new System.Drawing.Size(169, 6);
            // 
            // miSelectColor
            // 
            this.miSelectColor.Name = "miSelectColor";
            this.miSelectColor.Size = new System.Drawing.Size(172, 22);
            this.miSelectColor.Text = "miSelectColor";
            this.miSelectColor.Click += new System.EventHandler(this.miSelectColor_Click);
            // 
            // toolStripSeparator3
            // 
            this.toolStripSeparator3.Name = "toolStripSeparator3";
            this.toolStripSeparator3.Size = new System.Drawing.Size(6, 25);
            // 
            // tbBorders
            // 
            this.tbBorders.DropDown = this.MenuBorders;
            this.tbBorders.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.tbBorders.Name = "tbBorders";
            this.tbBorders.Size = new System.Drawing.Size(71, 22);
            this.tbBorders.Text = "tbBorders";
            // 
            // MenuBorders
            // 
            this.MenuBorders.Name = "MenuBorders";
            this.MenuBorders.Size = new System.Drawing.Size(61, 4);
            // 
            // TreeChartWin
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.ClientSize = new System.Drawing.Size(771, 362);
            this.Controls.Add(this.ToolBar1);
            this.Font = new System.Drawing.Font("Tahoma", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(204)));
            this.KeyPreview = true;
            this.Margin = new System.Windows.Forms.Padding(2);
            this.Name = "TreeChartWin";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "TreeChartWin";
            this.Title = "TreeChartWin";
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.TreeChartWin_KeyDown);
            this.Controls.SetChildIndex(this.ToolBar1, 0);
            this.ToolBar1.ResumeLayout(false);
            this.ToolBar1.PerformLayout();
            this.MenuModes.ResumeLayout(false);
            this.MenuPerson.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }
    }
}
