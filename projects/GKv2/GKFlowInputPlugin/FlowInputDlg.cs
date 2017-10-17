/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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

using GKCommon;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

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
        private readonly Plugin fPlugin;
        private readonly ILangMan fLangMan;
        private readonly IBaseWindow fBase;
        private readonly StringList fSourcesList;
        
        private GEDCOMSex fSimpleTempSex = GEDCOMSex.svMale;

        #region Instance control
        
        public FlowInputDlg(Plugin plugin, IBaseWindow baseWin)
        {
            InitializeComponent();

            //this.btnClose.Image = GKResources.iBtnCancel;

            fPlugin = plugin;
            fLangMan = plugin.LangMan;
            fBase = baseWin;
            fSourcesList = new StringList();

            cbEventType.Items.AddRange(new object[] {
                                           fPlugin.LangMan.LS(FLS.LSID_Birth),
                                           fPlugin.LangMan.LS(FLS.LSID_Death),
                                           fPlugin.LangMan.LS(FLS.LSID_Marriage) });

            PersonLinks = new FLS[]
            {
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

            for (PersonLink pl = PersonLink.plPerson; pl <= PersonLink.plChild; pl++)
            {
                cbPersonLink.Items.Add(fLangMan.LS(PersonLinks[(int)pl]));
            }

            SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
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

            for (PersonLink pl = PersonLink.plPerson; pl <= PersonLink.plLast; pl++)
            {
                if (fLangMan.LS(PersonLinks[(int)pl]) == aName)
                {
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

        public static DataGridViewColumn AddComboColumn(string colName, string headerText, object[] items)
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

            ((System.ComponentModel.ISupportInitialize)(dgv)).BeginInit();
            dgv.Columns.AddRange(new DataGridViewColumn[] {
                                     AddComboColumn("FLink", fLangMan.LS(FLS.LSID_Join), linksList),
                                     AddTextColumn("FName", fLangMan.LS(FLS.LSID_Name)),
                                     AddTextColumn("FPatronymic", fLangMan.LS(FLS.LSID_Patronymic)),
                                     AddTextColumn("FSurname", fLangMan.LS(FLS.LSID_Surname)),
                                     AddTextColumn("FAge", fLangMan.LS(FLS.LSID_Age)),
                                     AddTextColumn("FComment", fLangMan.LS(FLS.LSID_Comment))});
            ((System.ComponentModel.ISupportInitialize)(dgv)).EndInit();
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

            string fam = SysUtils.NormalizeName(tokens[0]);
            string nam = SysUtils.NormalizeName(tokens[1]);
            string pat = SysUtils.NormalizeName(tokens[2]);

            GEDCOMIndividualRecord iRec = fBase.Context.CreatePersonEx(nam, pat, fam, fSimpleTempSex, false);
            if (CheckBirth.Checked) {
                fBase.Context.CreateEventEx(iRec, "BIRT", GEDCOMDate.CreateByFormattedStr(EditBirthDate.Text, true), EditBirthPlace.Text);
            }

            if (CheckDeath.Checked) {
                fBase.Context.CreateEventEx(iRec, "DEAT", GEDCOMDate.CreateByFormattedStr(EditDeathDate.Text, true), EditDeathPlace.Text);
            }

            if (!string.IsNullOrEmpty(MemoNote.Text)) {
                GEDCOMNoteRecord noteRec = fBase.Context.Tree.CreateNote();
                noteRec.SetNoteText(MemoNote.Text);
                iRec.AddNote(noteRec);
            }

            fBase.NotifyRecord(iRec, RecordAction.raAdd);

            InitSimpleControls();
        }

        private void ParseSource()
        {
            int srcYear;
            if (!int.TryParse(edSourceYear.Text, out srcYear)) {
                ShowError(fLangMan.LS(FLS.LSID_SourceYearInvalid));
                return;
            }

            try
            {
                string srcName = cbSource.Text;
                string srcPage = edPage.Text;
                string place = edPlace.Text;

                GEDCOMIndividualRecord iMain = null;

                int num = dataGridView1.Rows.Count;
                for (int r = 0; r < num; r++)
                {
                    DataGridViewRow row = dataGridView1.Rows[r];

                    string lnk = (string)row.Cells[0].Value;
                    string nm = (string)row.Cells[1].Value;
                    string pt = (string)row.Cells[2].Value;
                    string fm = (string)row.Cells[3].Value;
                    string age = (string)row.Cells[4].Value;
                    string comment = (string)row.Cells[5].Value;

                    if (!string.IsNullOrEmpty(lnk)) {
                        PersonLink link = GetLinkByName(lnk);

                        GEDCOMSex sx = fBase.Context.DefineSex(nm, pt);
                        GEDCOMIndividualRecord iRec = fBase.Context.CreatePersonEx(nm, pt, fm, sx, false);

                        if (!string.IsNullOrEmpty(age) && SysUtils.IsDigits(age)) {
                            int birthYear = srcYear - int.Parse(age);
                            fBase.Context.CreateEventEx(iRec, "BIRT", "ABT "+birthYear.ToString(), "");
                        }

                        if (!string.IsNullOrEmpty(place)) {
                            GEDCOMCustomEvent evt = fBase.Context.CreateEventEx(iRec, "RESI", "", "");
                            evt.Place.StringValue = place;
                        }

                        if (!string.IsNullOrEmpty(comment)) {
                            GEDCOMNoteRecord noteRec = fBase.Context.Tree.CreateNote();
                            noteRec.SetNoteText(comment);
                            iRec.AddNote(noteRec);
                        }

                        if (!string.IsNullOrEmpty(srcName)) {
                            GEDCOMSourceRecord srcRec = fBase.Context.FindSource(srcName);
                            if (srcRec == null) {
                                srcRec = fBase.Context.Tree.CreateSource();
                                srcRec.FiledByEntry = srcName;
                            }
                            iRec.AddSource(srcRec, srcPage, 0);
                        }

                        fBase.NotifyRecord(iRec, RecordAction.raAdd);

                        GEDCOMFamilyRecord family = null;

                        if (link > PersonLink.plPerson && iMain == null) {
                            throw new PersonScanException(fLangMan.LS(FLS.LSID_BasePersonInvalid));
                        }

                        switch (link) {
                            case PersonLink.plNone:
                                break;

                            case PersonLink.plPerson:
                                {
                                    iMain = iRec;
                                    string evName = "";

                                    if (rbSK_Met.Checked) {
                                        switch (cbEventType.SelectedIndex) {
                                                case  0: evName = "BIRT"; break;
                                                case  1: evName = "DEAT"; break;
                                                case  2: evName = "MARR"; break;
                                        }
                                    }

                                    if (evName == "BIRT" || evName == "DEAT") {
                                        GEDCOMCustomEvent evt = fBase.Context.CreateEventEx(iRec, evName, GEDCOMDate.CreateByFormattedStr(edEventDate.Text, false), "");
                                        evt.Place.StringValue = place;
                                    } else if (evName == "MARR") {
                                        family = iRec.GetMarriageFamily(true);
                                        GEDCOMCustomEvent evt = fBase.Context.CreateEventEx(family, evName, GEDCOMDate.CreateByFormattedStr(edEventDate.Text, false), "");
                                        evt.Place.StringValue = place;
                                    }
                                }
                                break;

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
            finally
            {
                InitSourceControls();
            }
        }

        #endregion
        
        #region Form handlers
        
        private void btnParseClick(object sender, EventArgs e)
        {
            switch (PageControl1.SelectedIndex) {
                case 0:
                    ParseSimple();
                    break;
                case 1:
                    ParseSource();
                    break;
            }

            fBase.RefreshLists(false);
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
