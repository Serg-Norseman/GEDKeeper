/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public partial class ResearchEditDlg : CommonDialog<IResearchEditDlg, ResearchEditDlgController>, IResearchEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private GroupBox GroupBox1;
        private TextBox txtName;
        private Label lblName;
        private TabPage pageNotes;
        private TabPage pageTasks;
        private Button btnAccept;
        private Button btnCancel;
        private Label lblPriority;
        private ComboBox cmbPriority;
        private TabPage pageCommunications;
        private Label lblStatus;
        private ComboBox cmbStatus;
        private Label lblStartDate;
        private GKDateBox txtStartDate;
        private Label lblStopDate;
        private GKDateBox txtStopDate;
        private Label lblPercent;
        private NumericStepper nudPercent;
        private TabPage pageGroups;
        private GKSheetList fTasksList;
        private GKSheetList fCommunicationsList;
        private GKSheetList fGroupsList;
        private GKSheetList fNotesList;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMResearchRecord ResearchRecord
        {
            get { return fController.ResearchRecord; }
            set { fController.ResearchRecord = value; }
        }

        public ResearchEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            fController = new ResearchEditDlgController(this);
            fController.InitView(baseWin);
        }
    }
}
