#pragma warning disable IDE1006 // Naming Styles

using Terminal.Gui;

namespace GKUI.Forms
{
    partial class TreeChartWin
    {
        private MenuBar ToolBar1;
        private MenuBarItem tbImageSave;
        private ContextMenu MenuPerson;
        private MenuItem miEdit;
        private MenuItem miSpouseAdd;
        private MenuItem miSonAdd;
        private MenuItem miDaughterAdd;
        private MenuItem miFamilyAdd;
        private MenuItem miDelete;
        private MenuItem miRebuildKinships;
        private MenuBarItem tbModes;
        private MenuItem miModeBoth;
        private MenuItem miModeAncestors;
        private MenuItem miModeDescendants;
        private MenuItem miTraceSelected;
        private MenuItem miTraceKinships;
        private MenuItem miCertaintyIndex;
        private MenuItem miXRefVisible;
        private MenuItem miTrackSelectedLines;
        private MenuItem miTrackMatchedSources;
        private MenuItem miRebuildTree;
        private MenuItem miFillColor;
        private MenuItem miFatherAdd;
        private MenuItem miMotherAdd;
        private MenuBarItem tbOptions;
        private MenuBarItem tbFilter;
        private MenuBarItem tbPrev;
        private MenuBarItem tbNext;
        private MenuItem miSelectColor;
        private MenuItem miGoToRecord;
        private MenuBarItem tbGensAncestors;
        private MenuBarItem tbGensDescendants;
        private MenuItem miGoToPrimaryBranch;
        private MenuItem miOpenInNewWindow;
        private MenuItem miMergeDuplicates;
        private MenuItem miHideDescSpouses;
        private MenuItem miParentAges;

        private void InitializeComponent()
        {
            miModeBoth = new MenuItem();
            miModeBoth.Action += miModeItem_Click;

            miModeAncestors = new MenuItem();
            miModeAncestors.Action += miModeItem_Click;

            miModeDescendants = new MenuItem();
            miModeDescendants.Action += miModeItem_Click;

            miHideDescSpouses = new MenuItem();
            miHideDescSpouses.Action += miHideDescSpouses_Click;

            miTraceSelected = new MenuItem();
            miTraceSelected.Action += miTraceSelected_Click;

            miTraceKinships = new MenuItem();
            miTraceKinships.Action += miTraceKinships_Click;

            miCertaintyIndex = new MenuItem();
            miCertaintyIndex.Action += miCertaintyIndex_Click;

            miXRefVisible = new MenuItem();
            miXRefVisible.Action += miXRefVisible_Click;

            miTrackSelectedLines = new MenuItem();
            miTrackSelectedLines.Action += miTrackSelectedLines_Click;

            miTrackMatchedSources = new MenuItem();
            miTrackMatchedSources.Action += miTrackMatchedSources_Click;

            miParentAges = new MenuItem();
            miParentAges.Action += miParentAges_Click;

            miFillColor = new MenuItem();
            miFillColor.Action += miFillColor_Click;

            tbModes = new MenuBarItem();
            tbModes.Children.AddRange(new [] {
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
            });

            tbImageSave = new MenuBarItem();
            tbImageSave.Action += tbImageSave_Click;

            tbGensAncestors = new MenuBarItem();
            tbGensDescendants = new MenuBarItem();

            tbFilter = new MenuBarItem();
            tbFilter.Action += tbFilter_Click;

            tbPrev = new MenuBarItem();
            tbPrev.Action += tbPrev_Click;

            tbNext = new MenuBarItem();
            tbNext.Action += tbNext_Click;

            tbOptions = new MenuBarItem();
            tbOptions.Action += tbOptions_Click;

            miEdit = new MenuItem();
            miEdit.Action += miEdit_Click;

            miFatherAdd = new MenuItem();
            miFatherAdd.Action += miFatherAdd_Click;

            miMotherAdd = new MenuItem();
            miMotherAdd.Action += miMotherAdd_Click;

            miFamilyAdd = new MenuItem();
            miFamilyAdd.Action += miFamilyAdd_Click;

            miSpouseAdd = new MenuItem();
            miSpouseAdd.Action += miSpouseAdd_Click;

            miSonAdd = new MenuItem();
            miSonAdd.Action += miSonAdd_Click;

            miDaughterAdd = new MenuItem();
            miDaughterAdd.Action += miDaughterAdd_Click;

            miDelete = new MenuItem();
            miDelete.Action += miDelete_Click;

            miGoToRecord = new MenuItem();
            miGoToRecord.Action += miGoToRecord_Click;

            miGoToPrimaryBranch = new MenuItem();
            miGoToPrimaryBranch.Action += miGoToPrimaryBranch_Click;

            miOpenInNewWindow = new MenuItem();
            miOpenInNewWindow.Action += miOpenInNewWindow_Click;

            miMergeDuplicates = new MenuItem();
            miMergeDuplicates.Action += miMergeDuplicates_Click;

            miRebuildTree = new MenuItem();
            miRebuildTree.Shortcut = Key.F6;
            miRebuildTree.Action += miRebuildTree_Click;

            miRebuildKinships = new MenuItem();
            miRebuildKinships.Shortcut = Key.F7;
            miRebuildKinships.Action += miRebuildKinships_Click;

            miSelectColor = new MenuItem();
            miSelectColor.Action += miSelectColor_Click;

            MenuPerson = new ContextMenu();
            MenuPerson.Items.AddRange(new[] {
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
                miSelectColor
            });
            //MenuPerson.Opening += new System.ComponentModel.CancelEventHandler(MenuPerson_Opening);

            ToolBar1 = new MenuBar();
            ToolBar1.Menus = new MenuBarItem[] {
                tbImageSave,
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
