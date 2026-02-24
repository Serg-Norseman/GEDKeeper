using GKUI.Components;
using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TreeChartWin
    {
        private ToolStrip ToolBar1;
        private ToolStripButton tbImageSave;
        private ContextMenu MenuPerson;
        private ToolStripMenuItem miEdit;
        private ToolStripMenuItem miSpouseAdd;
        private ToolStripMenuItem miSonAdd;
        private ToolStripMenuItem miDaughterAdd;
        private ToolStripMenuItem miFamilyAdd;
        private ToolStripMenuItem miDelete;
        private ToolStripMenuItem miRebuildKinships;
        private ToolStripDropDownButton tbModes;
        private ToolStripMenuItem miModeBoth;
        private ToolStripMenuItem miModeAncestors;
        private ToolStripMenuItem miModeDescendants;
        private ToolStripMenuItem miTraceSelected;
        private ToolStripMenuItem miTraceKinships;
        private ToolStripMenuItem miCertaintyIndex;
        private ToolStripMenuItem miXRefVisible;
        private ToolStripMenuItem miTrackSelectedLines;
        private ToolStripMenuItem miTrackMatchedSources;
        private ToolStripMenuItem miRebuildTree;
        private ToolStripMenuItem miFillColor;
        private ToolStripMenuItem miFatherAdd;
        private ToolStripMenuItem miMotherAdd;
        private ToolStripButton tbOptions;
        private ToolStripButton tbFilter;
        private ToolStripButton tbPrev;
        private ToolStripButton tbNext;
        private ToolStripMenuItem miSelectColor;
        private ToolStripMenuItem miGoToRecord;
        private ToolStripDropDownButton tbGensCommon;
        private ToolStripDropDownButton tbGensAncestors;
        private ToolStripDropDownButton tbGensDescendants;
        private ToolStripMenuItem miGoToPrimaryBranch;
        private ToolStripMenuItem miOpenInNewWindow;
        private ToolStripMenuItem miMergeDuplicates;
        private ToolStripMenuItem miHideDescSpouses;
        private ToolStripMenuItem miParentAges;
        private MenuBarItem miMaps;
        private ToolStripMenuItem miMapAncestors;
        private ToolStripMenuItem miMapDescendants;
        private ToolStripMenuItem miMapAll;

        private void InitializeComponent()
        {
            miModeBoth = new ToolStripMenuItem();
            miModeBoth.Action += miModeItem_Click;

            miModeAncestors = new ToolStripMenuItem();
            miModeAncestors.Action += miModeItem_Click;

            miModeDescendants = new ToolStripMenuItem();
            miModeDescendants.Action += miModeItem_Click;

            miHideDescSpouses = new ToolStripMenuItem();
            miHideDescSpouses.Action += miHideDescSpouses_Click;

            miTraceSelected = new ToolStripMenuItem();
            miTraceSelected.Action += miTraceSelected_Click;

            miTraceKinships = new ToolStripMenuItem();
            miTraceKinships.Action += miTraceKinships_Click;

            miCertaintyIndex = new ToolStripMenuItem();
            miCertaintyIndex.Action += miCertaintyIndex_Click;

            miXRefVisible = new ToolStripMenuItem();
            miXRefVisible.Action += miXRefVisible_Click;

            miTrackSelectedLines = new ToolStripMenuItem();
            miTrackSelectedLines.Action += miTrackSelectedLines_Click;

            miTrackMatchedSources = new ToolStripMenuItem();
            miTrackMatchedSources.Action += miTrackMatchedSources_Click;

            miParentAges = new ToolStripMenuItem();
            miParentAges.Action += miParentAges_Click;

            miFillColor = new ToolStripMenuItem();
            miFillColor.Action += miFillColor_Click;

            tbModes = new ToolStripDropDownButton();
            tbModes.Children = new ToolStripItem[] {
                miModeBoth,
                miModeAncestors,
                miModeDescendants,
                null,
                miHideDescSpouses,
                null,
                miTraceSelected,
                miTraceKinships,
                miCertaintyIndex,
                miXRefVisible,
                miTrackSelectedLines,
                miTrackMatchedSources,
                miParentAges,
                null,
                miFillColor,
            };

            tbImageSave = new ToolStripButton();
            tbImageSave.Action += tbImageSave_Click;

            tbGensCommon = new ToolStripDropDownButton();
            tbGensAncestors = new ToolStripDropDownButton();
            tbGensDescendants = new ToolStripDropDownButton();

            tbFilter = new ToolStripButton();
            tbFilter.Action += tbFilter_Click;

            tbPrev = new ToolStripButton();
            tbPrev.Action += tbPrev_Click;

            tbNext = new ToolStripButton();
            tbNext.Action += tbNext_Click;

            tbOptions = new ToolStripButton();
            tbOptions.Action += tbOptions_Click;

            miEdit = new ToolStripMenuItem();
            miEdit.Action += miEdit_Click;

            miFatherAdd = new ToolStripMenuItem();
            miFatherAdd.Action += miFatherAdd_Click;

            miMotherAdd = new ToolStripMenuItem();
            miMotherAdd.Action += miMotherAdd_Click;

            miFamilyAdd = new ToolStripMenuItem();
            miFamilyAdd.Action += miFamilyAdd_Click;

            miSpouseAdd = new ToolStripMenuItem();
            miSpouseAdd.Action += miSpouseAdd_Click;

            miSonAdd = new ToolStripMenuItem();
            miSonAdd.Action += miSonAdd_Click;

            miDaughterAdd = new ToolStripMenuItem();
            miDaughterAdd.Action += miDaughterAdd_Click;

            miDelete = new ToolStripMenuItem();
            miDelete.Action += miDelete_Click;

            miGoToRecord = new ToolStripMenuItem();
            miGoToRecord.Action += miGoToRecord_Click;

            miGoToPrimaryBranch = new ToolStripMenuItem();
            miGoToPrimaryBranch.Action += miGoToPrimaryBranch_Click;

            miOpenInNewWindow = new ToolStripMenuItem();
            miOpenInNewWindow.Action += miOpenInNewWindow_Click;

            miMergeDuplicates = new ToolStripMenuItem();
            miMergeDuplicates.Action += miMergeDuplicates_Click;

            miRebuildTree = new ToolStripMenuItem();
            miRebuildTree.Shortcut = Key.F6;
            miRebuildTree.Action += miRebuildTree_Click;

            miRebuildKinships = new ToolStripMenuItem();
            miRebuildKinships.Shortcut = Key.F7;
            miRebuildKinships.Action += miRebuildKinships_Click;

            miMapAncestors = new ToolStripMenuItem();
            miMapAncestors.Action += miMapAncestors_Click;

            miMapDescendants = new ToolStripMenuItem();
            miMapDescendants.Action += miMapDescendants_Click;

            miMapAll = new ToolStripMenuItem();
            miMapAll.Action += miMapAll_Click;

            miMaps = new MenuBarItem();
            miMaps.Children = new ToolStripItem[] {
                miMapAncestors,
                miMapDescendants,
                miMapAll
            };

            miSelectColor = new ToolStripMenuItem();
            miSelectColor.Action += miSelectColor_Click;

            MenuPerson = new ContextMenu();
            MenuPerson.MenuItems = new MenuBarItem("Actions", new MenuItem[] {
                miEdit,
                null,
                miFatherAdd,
                miMotherAdd,
                miFamilyAdd,
                miSpouseAdd,
                miSonAdd,
                miDaughterAdd,
                null,
                miDelete,
                null,
                miGoToRecord,
                miGoToPrimaryBranch,
                miOpenInNewWindow,
                miMergeDuplicates,
                null,
                miRebuildTree,
                miRebuildKinships,
                null,
                miMaps,
                null,
                miSelectColor
            });
            //MenuPerson.Opening += new System.ComponentModel.CancelEventHandler(MenuPerson_Opening);

            ToolBar1 = new ToolStrip();
            ToolBar1.Menus = new ToolStripButton[] {
                tbImageSave,
                tbGensCommon,
                tbGensAncestors,
                tbGensDescendants,
                tbModes,
                tbFilter,
                tbPrev,
                tbNext,
                tbOptions,
            };
            Add(ToolBar1);

            Width = Dim.Fill();
            Height = Dim.Fill();
            KeyDown += TreeChartWin_KeyDown;
        }
    }
}
