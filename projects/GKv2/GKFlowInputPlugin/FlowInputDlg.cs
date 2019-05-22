/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;
using GKUI.Components;

namespace GKFlowInputPlugin
{
    [Serializable]
    public class PersonScanException : Exception
    {
        public PersonScanException()
        {
        }

        public PersonScanException(string message) : base(message)
        {
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public partial class FlowInputDlg : Form
    {
        private enum PersonLink
        {
            plNone,
            plPerson,
            plFather,
            plMother,
            plGodparent,
            plSpouse,
            plChild,
            
            plLast = plChild
        }

        private readonly FLS[] PersonLinks;
        private readonly IPlugin fPlugin;
        private readonly ILangMan fLangMan;
        private readonly IBaseWindow fBase;
        private readonly StringList fSourcesList;
        
        private GEDCOMSex fSimpleTempSex = GEDCOMSex.svMale;

        #region Instance control
        
        public FlowInputDlg(IPlugin plugin, IBaseWindow baseWin)
        {
            InitializeComponent();

            btnClose.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fPlugin = plugin;
            fLangMan = plugin.LangMan;
            fBase = baseWin;
            fSourcesList = new StringList();

            cbEventType.Items.AddRange(new object[] {
                                           fPlugin.LangMan.LS(FLS.LSID_Birth),
                                           fPlugin.LangMan.LS(FLS.LSID_Death),
                                           fPlugin.LangMan.LS(FLS.LSID_Marriage) });

            PersonLinks = new FLS[] {
                FLS.LSID_RK_Unk,
                FLS.LSID_PLPerson,
                FLS.LSID_Father,
                FLS.LSID_Mother,
                FLS.LSID_PLGodparent,
                FLS.LSID_Spouse,
                FLS.LSID_Child
            };

            InitGrid(dataGridView1);
            InitSimpleControls();
            InitSourceControls();

            for (PersonLink pl = PersonLink.plPerson; pl <= PersonLink.plChild; pl++) {
                cbPersonLink.Items.Add(fLangMan.LS(PersonLinks[(int)pl]));
            }

            SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fSourcesList.Dispose();
            }
            base.Dispose(disposing);
        }

        #endregion

        #region ILocalization support

        public void SetLang()
        {
            btnParse.Text = fLangMan.LS(FLS.LSID_DlgAppend);
            btnClose.Text = fLangMan.LS(FLS.LSID_DlgClose);
            Text = fLangMan.LS(FLS.LSID_PluginTitle);
            tsSimpleInput.Text = fLangMan.LS(FLS.LSID_InputSimple);
            btnMale.Text = new string(fLangMan.LS(FLS.LSID_SexM)[0], 1);
            //this.btnFemale.Text = new string(LangMan.LS(FLS.67][0], 1);
            lblFullName.Text = fLangMan.LS(FLS.LSID_FullName);
            CheckBirth.Text = fLangMan.LS(FLS.LSID_Birth);
            lblBirthDate.Text = fLangMan.LS(FLS.LSID_BirthDate);
            lblBirthPlace.Text = fLangMan.LS(FLS.LSID_BirthPlace);
            CheckDeath.Text = fLangMan.LS(FLS.LSID_Death);
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

        #endregion

        #region Utilities
        
        private void ShowError(string msg)
        {
            string title = fLangMan.LS(FLS.LSID_PluginTitle);
            MessageBox.Show(msg, title, MessageBoxButtons.OK, MessageBoxIcon.Hand);
        }

        private PersonLink GetLinkByName(string aName)
        {
            PersonLink res = PersonLink.plNone;

            for (PersonLink pl = PersonLink.plPerson; pl <= PersonLink.plLast; pl++) {
                if (fLangMan.LS(PersonLinks[(int)pl]) == aName) {
                    res = pl;
                    break;
                }
            }

            return res;
        }

        public static DataGridViewColumn AddTextColumn(string colName, string headerText)
        {
            DataGridViewColumn col = new DataGridViewTextBoxColumn();
            col.HeaderText = headerText;
            col.Name = colName;
            return col;
        }

        public static DataGridViewComboBoxColumn AddComboColumn(string colName, string headerText, object[] items)
        {
            DataGridViewComboBoxColumn col = new DataGridViewComboBoxColumn();
            col.HeaderText = headerText;
            col.Name = colName;
            col.Items.AddRange(items);
            return col;
        }

        #endregion
        
        #region Initialize controls
        
        private void InitSimpleControls()
        {
            EditName.Text = "";
            EditBirthDate.Text = "";
            EditBirthPlace.Text = "";
            CheckBirth.Checked = false;
            EditDeathDate.Text = "";
            EditDeathPlace.Text = "";
            CheckDeath.Checked = false;
            MemoNote.Text = "";

            fSimpleTempSex = GEDCOMSex.svMale;
            btnMale.Text = new string(fLangMan.LS(FLS.LSID_SexM)[0], 1);
        }

        private void InitSourceControls()
        {
            fBase.Context.GetSourcesList(fSourcesList);

            cbSource.Items.Clear();

            int num = fSourcesList.Count;
            for (int i = 0; i < num; i++) {
                cbSource.Items.Add(fSourcesList[i]);
            }

            edPage.Text = "";
            edSourceYear.Text = "";
            edPlace.Text = "";
            edEventDate.Text = "";
            cbEventType.SelectedIndex = -1;
            dataGridView1.Rows.Clear();
        }

        private void InitGrid(DataGridView dgv)
        {
            int num = PersonLinks.Length;
            object[] linksList = new object[num];
            for (int i = 0; i < num; i++) linksList[i] = fLangMan.LS(PersonLinks[i]);

            string[] namesList = new string[0];
            string[] patrList = new string[0];
            string[] surnamesList = new string[0];

            dgv.Columns.AddRange(new DataGridViewColumn[] {
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
            dgv.EditMode = DataGridViewEditMode.EditOnEnter;
        }

        private DataGridViewComboBoxColumn GetComboBoxColumn(int columnIndex)
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
        }

        #endregion

        #region Parse functions

        private void ParseSimple()
        {
            string tmp = EditName.Text.ToLower();
            string[] tokens = tmp.Split(' ');
            if (tokens.Length < 3) {
                ShowError(fLangMan.LS(FLS.LSID_NameInvalid));
                return;
            }

            string fam = ConvertHelper.UniformName(tokens[0]);
            string nam = ConvertHelper.UniformName(tokens[1]);
            string pat = ConvertHelper.UniformName(tokens[2]);

            GDMIndividualRecord iRec = fBase.Context.CreatePersonEx(nam, pat, fam, fSimpleTempSex, false);
            if (CheckBirth.Checked) {
                fBase.Context.CreateEventEx(iRec, GEDCOMTagType.BIRT, GDMDate.CreateByFormattedStr(EditBirthDate.Text, true), EditBirthPlace.Text);
            }

            if (CheckDeath.Checked) {
                fBase.Context.CreateEventEx(iRec, GEDCOMTagType.DEAT, GDMDate.CreateByFormattedStr(EditDeathDate.Text, true), EditDeathPlace.Text);
            }

            if (!string.IsNullOrEmpty(MemoNote.Text)) {
                GDMNoteRecord noteRec = fBase.Context.Tree.CreateNote();
                noteRec.SetNoteText(MemoNote.Text);
                iRec.AddNote(noteRec);
            }

            fBase.NotifyRecord(iRec, RecordAction.raAdd);

            InitSimpleControls();
        }

        private static string CheckStr(string val)
        {
            return (val == null) ? string.Empty : val;
        }

        // TODO: rollback changes when exception!
        private void ParseSource()
        {
            int srcYear;
            if (!int.TryParse(edSourceYear.Text, out srcYear)) {
                ShowError(fLangMan.LS(FLS.LSID_SourceYearInvalid));
                return;
            }

            string srcName = cbSource.Text;
            string srcPage = edPage.Text;
            string place = edPlace.Text;

            GDMSourceRecord srcRec = null;
            if (!string.IsNullOrEmpty(srcName)) {
                srcRec = fBase.Context.FindSource(srcName);
                if (srcRec == null) {
                    srcRec = fBase.Context.Tree.CreateSource();
                    srcRec.ShortTitle = srcName;
                }
            }

            GDMIndividualRecord iMain = null;

            int num = dataGridView1.Rows.Count;
            for (int r = 0; r < num; r++)
            {
                DataGridViewRow row = dataGridView1.Rows[r];

                string lnk = CheckStr((string)row.Cells[0].Value);
                string nm = CheckStr((string)row.Cells[1].Value);
                string pt = CheckStr((string)row.Cells[2].Value);
                string fm = CheckStr((string)row.Cells[3].Value);
                string age = CheckStr((string)row.Cells[4].Value);
                string comment = CheckStr((string)row.Cells[5].Value);

                if (!string.IsNullOrEmpty(lnk)) {
                    PersonLink link = GetLinkByName(lnk);
                    if (link == PersonLink.plNone) continue;

                    GEDCOMSex sx = fBase.Context.DefineSex(nm, pt);
                    GDMIndividualRecord iRec = fBase.Context.CreatePersonEx(nm, pt, fm, sx, false);

                    if (!string.IsNullOrEmpty(age) && ConvertHelper.IsDigits(age)) {
                        int birthYear = srcYear - int.Parse(age);
                        fBase.Context.CreateEventEx(iRec, GEDCOMTagType.BIRT, "ABT "+birthYear.ToString(), "");
                    }

                    if (!string.IsNullOrEmpty(place)) {
                        GDMCustomEvent evt = fBase.Context.CreateEventEx(iRec, GEDCOMTagType.RESI, "", "");
                        evt.Place.StringValue = place;
                    }

                    if (!string.IsNullOrEmpty(comment)) {
                        GDMNoteRecord noteRec = fBase.Context.Tree.CreateNote();
                        noteRec.SetNoteText(comment);
                        iRec.AddNote(noteRec);
                    }

                    if (srcRec != null) {
                        iRec.AddSource(srcRec, srcPage, 0);
                    }

                    fBase.NotifyRecord(iRec, RecordAction.raAdd);

                    GDMFamilyRecord family = null;

                    if (link == PersonLink.plPerson) {

                        iMain = iRec;
                        string evName = "";

                        if (rbSK_Met.Checked) {
                            switch (cbEventType.SelectedIndex) {
                                case  0:
                                    evName = GEDCOMTagType.BIRT;
                                    break;
                                case  1:
                                    evName = GEDCOMTagType.DEAT;
                                    break;
                                case  2:
                                    evName = GEDCOMTagType.MARR;
                                    break;
                            }
                        }

                        if (evName == GEDCOMTagType.BIRT || evName == GEDCOMTagType.DEAT) {
                            GDMCustomEvent evt = fBase.Context.CreateEventEx(iRec, evName, GDMDate.CreateByFormattedStr(edEventDate.Text, false), "");
                            evt.Place.StringValue = place;
                        } else if (evName == GEDCOMTagType.MARR) {
                            family = iRec.GetMarriageFamily(true);
                            GDMCustomEvent evt = fBase.Context.CreateEventEx(family, evName, GDMDate.CreateByFormattedStr(edEventDate.Text, false), "");
                            evt.Place.StringValue = place;
                        }

                    } else {

                        if (iMain == null) {
                            throw new PersonScanException(fLangMan.LS(FLS.LSID_BasePersonInvalid));
                        } else {
                            switch (link) {
                                case PersonLink.plFather:
                                case PersonLink.plMother:
                                    family = iMain.GetParentsFamily(true);
                                    family.AddSpouse(iRec);
                                    break;

                                case PersonLink.plGodparent:
                                    iMain.AddAssociation(fLangMan.LS(FLS.LSID_PLGodparent), iRec);
                                    break;

                                case PersonLink.plSpouse:
                                    family = iMain.GetMarriageFamily(true);
                                    family.AddSpouse(iRec);
                                    break;

                                case PersonLink.plChild:
                                    family = iMain.GetMarriageFamily(true);
                                    family.AddChild(iRec);
                                    break;
                            }
                        }

                    }
                }
            }

            InitSourceControls();
        }

        #endregion

        #region Form handlers

        private void btnParseClick(object sender, EventArgs e)
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
                ShowError(ex.Message);
            }
        }

        private void BtnMaleClick(object sender, EventArgs e)
        {
            switch (fSimpleTempSex) {
                case GEDCOMSex.svMale:
                    btnMale.Text = new string(fLangMan.LS(FLS.LSID_SexF)[0], 1);
                    fSimpleTempSex = GEDCOMSex.svFemale;
                    break;
                case GEDCOMSex.svFemale:
                    btnMale.Text = new string(fLangMan.LS(FLS.LSID_SexM)[0], 1);
                    fSimpleTempSex = GEDCOMSex.svMale;
                    break;
            }
        }

        private void EditBirthDateTextChanged(object sender, EventArgs e)
        {
            CheckBirth.Checked = true;
        }

        private void EditDeathDateTextChanged(object sender, EventArgs e)
        {
            CheckDeath.Checked = true;
        }

        private void RadioButton1CheckedChanged(object sender, EventArgs e)
        {
            gbMetrics.Enabled = (rbSK_Met.Checked);
        }

        #endregion
    }
}
