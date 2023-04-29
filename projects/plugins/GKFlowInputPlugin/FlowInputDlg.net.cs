/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using BSLib;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Interfaces;
using GKUI.Components;
using GKUI.Forms;

namespace GKFlowInputPlugin
{
    public partial class FlowInputDlg : CommonForm
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnParse;
        private Button btnClose;
        private TabControl PageControl1;
        private TabPage tsSimpleInput;
        private TabPage tsSourceInput;
        private Label lblFullName;
        private Label lblNote;
        private Button btnMale;
        private TextBox txtFullName;
        private TextArea txtNote;
        private GroupBox Panel1;
        private Label lblBirthDate;
        private Label lblBirthPlace;
        private MaskedTextBox txtBirthDate;
        private TextBox txtBirthPlace;
        private CheckBox chkBirth;
        private GroupBox Panel2;
        private Label lblDeathDate;
        private Label lblDeathPlace;
        private CheckBox chkDeath;
        private MaskedTextBox txtDeathDate;
        private TextBox txtDeathPlace;
        private Label lblSource;
        private Label lblPage;
        private Label lblYear;
        private Label lblSettlement;
        private ComboBox cbSource;
        private TextBox edPage;
        private TextBox edSourceYear;
        private TextBox edPlace;
        private ComboBox cbPersonLink;
        private GroupBox rgSourceKind;
        private GroupBox gbMetrics;
        private Label lblEventDate;
        private Label lblEventType;
        private MaskedTextBox edEventDate;
        private ComboBox cbEventType;
        private GridView dataGridView1;
        private RadioButton rbSK_Rev;
        private RadioButton rbSK_Met;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        private readonly IPlugin fPlugin;
        private readonly ILangMan fLangMan;
        private readonly IBaseWindow fBase;
        private GDMSex fSimpleTempSex = GDMSex.svMale;
        private readonly FlowInput fFlowInput;

        public FlowInputDlg(IPlugin plugin, IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            UIHelper.FixRadioButtons(this, rgSourceKind);

            fPlugin = plugin;
            fLangMan = plugin.LangMan;
            fBase = baseWin;
            fFlowInput = new FlowInput(plugin, baseWin);

            cbEventType.Items.AddRange(new string[] { fPlugin.LangMan.LS(FLS.LSID_Birth), fPlugin.LangMan.LS(FLS.LSID_Death), fPlugin.LangMan.LS(FLS.LSID_Marriage) });

            InitGrid(dataGridView1);
            InitSimpleControls();
            InitSourceControls();

            /*
            this.dataGridView1.AllowUserToResizeRows = false;
            this.dataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.dataGridView1.MultiSelect = false;

            this.cbPersonLink.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cbPersonLink.Size = new System.Drawing.Size(145, 21);
            this.cbPersonLink.Visible = false;
             */
            /*for (PersonLink pl = PersonLink.plPerson; pl <= PersonLink.plChild; pl++) {
                cbPersonLink.Items.Add(fLangMan.LS(FlowInput.PersonLinks[(int)pl]));
            }*/

            SetLocale();
        }

        public void SetLocale()
        {
            btnParse.Text = fLangMan.LS(FLS.LSID_DlgAppend);
            btnClose.Text = fLangMan.LS(FLS.LSID_DlgClose);
            Title = fLangMan.LS(FLS.LSID_PluginTitle);
            tsSimpleInput.Text = fLangMan.LS(FLS.LSID_InputSimple);
            btnMale.Text = new string(fLangMan.LS(FLS.LSID_SexM)[0], 1);
            //this.btnFemale.Text = new string(LangMan.LS(FLS.67][0], 1);
            lblFullName.Text = fLangMan.LS(FLS.LSID_FullName);
            chkBirth.Text = fLangMan.LS(FLS.LSID_Birth);
            lblBirthDate.Text = fLangMan.LS(FLS.LSID_BirthDate);
            lblBirthPlace.Text = fLangMan.LS(FLS.LSID_BirthPlace);
            chkDeath.Text = fLangMan.LS(FLS.LSID_Death);
            lblDeathDate.Text = fLangMan.LS(FLS.LSID_DeathDate);
            lblDeathPlace.Text = fLangMan.LS(FLS.LSID_DeathPlace);
            lblNote.Text = fLangMan.LS(FLS.LSID_Note);
            tsSourceInput.Text = fLangMan.LS(FLS.LSID_InputSource);
            rgSourceKind.Text = fLangMan.LS(FLS.LSID_SourceKind);
            lblSource.Text = fLangMan.LS(FLS.LSID_Source);
            lblPage.Text = fLangMan.LS(FLS.LSID_Page);
            lblYear.Text = fLangMan.LS(FLS.LSID_Year);
            lblSettlement.Text = fLangMan.LS(FLS.LSID_Settlement);
            gbMetrics.Text = fLangMan.LS(FLS.LSID_SK_Met);
            lblEventDate.Text = fLangMan.LS(FLS.LSID_EventDate);
            lblEventType.Text = fLangMan.LS(FLS.LSID_EventType);

            rbSK_Rev.Text = fLangMan.LS(FLS.LSID_SK_Rev);
            rbSK_Met.Text = fLangMan.LS(FLS.LSID_SK_Met);
        }

        #region Parse functions

        private void ParseSimple()
        {
            fFlowInput.ParseSimple(txtFullName.Text, fSimpleTempSex,
                chkBirth.Checked.Value, txtBirthDate.Text, txtBirthPlace.Text,
                chkDeath.Checked.Value, txtDeathDate.Text, txtDeathPlace.Text,
                txtNote.Text);

            InitSimpleControls();
        }

        // TODO: rollback changes when exception!
        private void ParseSource()
        {
            int srcYear;
            if (!int.TryParse(edSourceYear.Text, out srcYear)) {
                fFlowInput.ShowError(fLangMan.LS(FLS.LSID_SourceYearInvalid));
                return;
            }

            GDMSourceRecord srcRec = fFlowInput.InitializeSource(cbSource.Text);
            if (srcRec == null)
                return;

            string srcPage = edPage.Text;
            string place = edPlace.Text;

            int eventType = -1;
            if (rbSK_Met.Checked) {
                eventType = cbEventType.SelectedIndex;
            }
            string eventDate = edEventDate.Text;

            /*GDMIndividualRecord iMain = null;

            int num = dataGridView1.Rows.Count;
            for (int r = 0; r < num; r++) {
                DataGridViewRow row = dataGridView1.Rows[r];
                string lnk = (string)row.Cells[0].Value;
                string nm = (string)row.Cells[1].Value;
                string pt = (string)row.Cells[2].Value;
                string fm = (string)row.Cells[3].Value;
                string age = (string)row.Cells[4].Value;
                string comment = (string)row.Cells[5].Value;

                fFlowInput.ParseSource(srcRec, srcYear, srcPage, place, ref iMain, lnk, nm, pt, fm, age, comment, eventType, eventDate);
            }*/

            InitSourceControls();
        }

        #endregion

        #region Form handlers

        /*private static DataGridViewColumn AddTextColumn(string colName, string headerText)
        {
            DataGridViewColumn col = new DataGridViewTextBoxColumn();
            col.HeaderText = headerText;
            col.Name = colName;
            return col;
        }

        private static DataGridViewComboBoxColumn AddComboColumn(string colName, string headerText, object[] items)
        {
            DataGridViewComboBoxColumn col = new DataGridViewComboBoxColumn();
            col.HeaderText = headerText;
            col.Name = colName;
            col.Items.AddRange(items);
            return col;
        }*/

        private void InitSimpleControls()
        {
            txtFullName.Text = "";
            txtBirthDate.Text = "";
            txtBirthPlace.Text = "";
            chkBirth.Checked = false;
            txtDeathDate.Text = "";
            txtDeathPlace.Text = "";
            chkDeath.Checked = false;
            txtNote.Text = "";

            fSimpleTempSex = GDMSex.svMale;
            btnMale.Text = new string(fLangMan.LS(FLS.LSID_SexM)[0], 1);
        }

        private void InitSourceControls()
        {
            var sourcesList = new StringList();
            fBase.Context.GetSourcesList(sourcesList);
            cbSource.Items.Clear();
            for (int i = 0; i < sourcesList.Count; i++) {
                cbSource.Items.Add(sourcesList[i]);
            }

            edPage.Text = "";
            edSourceYear.Text = "";
            edPlace.Text = "";
            edEventDate.Text = "";
            cbEventType.SelectedIndex = -1;
            //dataGridView1.Rows.Clear();
        }

        private void InitGrid(GridView dgv)
        {
            int num = FlowInput.PersonLinks.Length;
            object[] linksList = new object[num];
            for (int i = 0; i < num; i++) linksList[i] = fLangMan.LS(FlowInput.PersonLinks[i]);

            string[] namesList = new string[0];
            string[] patrList = new string[0];
            string[] surnamesList = new string[0];

            /*dgv.Columns.AddRange(new DataGridViewColumn[] {
                                     AddComboColumn("FLink", fLangMan.LS(FLS.LSID_Join), linksList),

                                     AddComboColumn("FName", fLangMan.LS(FLS.LSID_Name), namesList),
                                     AddComboColumn("FPatronymic", fLangMan.LS(FLS.LSID_Patronymic), patrList),
                                     AddComboColumn("FSurname", fLangMan.LS(FLS.LSID_Surname), surnamesList),
                                     //AddTextColumn("FName", fLangMan.LS(FLS.LSID_Name)),
                                     //AddTextColumn("FPatronymic", fLangMan.LS(FLS.LSID_Patronymic)),
                                     //AddTextColumn("FSurname", fLangMan.LS(FLS.LSID_Surname)),

                                     AddTextColumn("FAge", fLangMan.LS(FLS.LSID_Age)),
                                     AddTextColumn("FComment", fLangMan.LS(FLS.LSID_Comment))});

            dgv.CellValidating += dataGridView1_CellValidating;
            dgv.EditingControlShowing += dataGridView1_EditingControlShowing;
            dgv.EditMode = DataGridViewEditMode.EditOnEnter;*/
        }

        /*private DataGridViewComboBoxColumn GetComboBoxColumn(int columnIndex)
        {
            if (columnIndex >= 1 && columnIndex <= 3) {
                return dataGridView1.Columns[columnIndex] as DataGridViewComboBoxColumn;
            } else {
                return null;
            }
        }

        private void dataGridView1_CellValidating(object sender, DataGridViewCellValidatingEventArgs e)
        {
            var comboColumn = GetComboBoxColumn(e.ColumnIndex);
            if (comboColumn != null) {
                DataGridViewComboBoxCell cell = dataGridView1.CurrentCell as DataGridViewComboBoxCell;
                if (!comboColumn.Items.Contains(e.FormattedValue)) {
                    comboColumn.Items.Add(e.FormattedValue);
                }
                if (dataGridView1.IsCurrentCellDirty) {
                    dataGridView1.CommitEdit(DataGridViewDataErrorContexts.Commit);
                    dataGridView1.EndEdit();
                }
                cell.Value = e.FormattedValue;
            }
        }

        private void dataGridView1_EditingControlShowing(object sender, DataGridViewEditingControlShowingEventArgs e)
        {
            var comboColumn = GetComboBoxColumn(dataGridView1.CurrentCellAddress.X);
            if (comboColumn != null) {
                ComboBox cb = e.Control as ComboBox;
                if (cb != null) {
                    cb.DropDownStyle = ComboBoxStyle.DropDown;
                }
            }
        }*/

        private void btnParse_Click(object sender, EventArgs e)
        {
            try {
                try {
                    switch (PageControl1.SelectedIndex) {
                        case 0:
                            ParseSimple();
                            break;

                        case 1:
                            ParseSource();
                            break;
                    }
                } finally {
                    fBase.RefreshLists(false);
                }
            } catch (Exception ex) {
                fFlowInput.ShowError(ex.Message);
            }
        }

        private void btnSex_Click(object sender, EventArgs e)
        {
            switch (fSimpleTempSex) {
                case GDMSex.svMale:
                    btnMale.Text = new string(fLangMan.LS(FLS.LSID_SexF)[0], 1);
                    fSimpleTempSex = GDMSex.svFemale;
                    break;
                case GDMSex.svFemale:
                    btnMale.Text = new string(fLangMan.LS(FLS.LSID_SexM)[0], 1);
                    fSimpleTempSex = GDMSex.svMale;
                    break;
            }
        }

        private void txtBirthDate_TextChanged(object sender, EventArgs e)
        {
            chkBirth.Checked = true;
        }

        private void txtDeathDate_TextChanged(object sender, EventArgs e)
        {
            chkDeath.Checked = true;
        }

        private void rbX_CheckedChanged(object sender, EventArgs e)
        {
            gbMetrics.Enabled = (rbSK_Met.Checked);
        }

        private void btnClose_Click(object sender, EventArgs e)
        {
            Close();
        }

        #endregion
    }
}
