/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
        
        public FlowInputDlg(Plugin plugin, IBaseWindow aBase)
        {
            this.InitializeComponent();

            //this.btnClose.Image = global::GKResources.iBtnCancel;

            this.fPlugin = plugin;
            this.fLangMan = plugin.LangMan;
            this.fBase = aBase;
            this.fSourcesList = new StringList();

            this.cbEventType.Items.AddRange(new object[] {
                                                fPlugin.LangMan.LS(FLS.LSID_Birth),
                                                fPlugin.LangMan.LS(FLS.LSID_Death),
                                                fPlugin.LangMan.LS(FLS.LSID_Marriage) });

            this.PersonLinks = new FLS[]
            {
                FLS.LSID_RK_Unk,
                FLS.LSID_PLPerson,
                FLS.LSID_Father,
                FLS.LSID_Mother,
                FLS.LSID_PLGodparent,
                FLS.LSID_Spouse,
                FLS.LSID_Child
            };

            this.InitGrid(dataGridView1);
            this.InitSimpleControls();
            this.InitSourceControls();

            for (PersonLink pl = PersonLink.plPerson; pl <= PersonLink.plChild; pl++)
            {
                this.cbPersonLink.Items.Add(fLangMan.LS(this.PersonLinks[(int)pl]));
            }

            this.SetLang();
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                this.fSourcesList.Dispose();
            }
            base.Dispose(disposing);
        }

        #endregion

        #region Utilities
        
        public void SetLang()
        {
            this.btnParse.Text = fLangMan.LS(FLS.LSID_DlgAppend);
            this.btnClose.Text = fLangMan.LS(FLS.LSID_DlgClose);
            this.Text = fLangMan.LS(FLS.LSID_PluginTitle);
            this.tsSimpleInput.Text = fLangMan.LS(FLS.LSID_InputSimple);
            this.btnMale.Text = new string(fLangMan.LS(FLS.LSID_SexM)[0], 1);
            //this.btnFemale.Text = new string(LangMan.LS(FLS.67][0], 1);
            this.lblFullName.Text = fLangMan.LS(FLS.LSID_FullName);
            this.CheckBirth.Text = fLangMan.LS(FLS.LSID_Birth);
            this.lblBirthDate.Text = fLangMan.LS(FLS.LSID_BirthDate);
            this.lblBirthPlace.Text = fLangMan.LS(FLS.LSID_BirthPlace);
            this.CheckDeath.Text = fLangMan.LS(FLS.LSID_Death);
            this.lblDeathDate.Text = fLangMan.LS(FLS.LSID_DeathDate);
            this.lblDeathPlace.Text = fLangMan.LS(FLS.LSID_DeathPlace);
            this.lblNote.Text = fLangMan.LS(FLS.LSID_Note);
            this.tsSourceInput.Text = fLangMan.LS(FLS.LSID_InputSource);
            this.rgSourceKind.Text = fLangMan.LS(FLS.LSID_SourceKind);
            this.lblSource.Text = fLangMan.LS(FLS.LSID_Source);
            this.lblPage.Text = fLangMan.LS(FLS.LSID_Page);
            this.lblYear.Text = fLangMan.LS(FLS.LSID_Year);
            this.lblSettlement.Text = fLangMan.LS(FLS.LSID_Settlement);
            this.gbMetrics.Text = fLangMan.LS(FLS.LSID_SK_Met);
            this.lblEventDate.Text = fLangMan.LS(FLS.LSID_EventDate);
            this.lblEventType.Text = fLangMan.LS(FLS.LSID_EventType);
            
            this.rbSK_Rev.Text = fLangMan.LS(FLS.LSID_SK_Rev);
            this.rbSK_Met.Text = fLangMan.LS(FLS.LSID_SK_Met);
        }
        
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
                if (this.fLangMan.LS(PersonLinks[(int)pl]) == aName)
                {
                    res = pl;
                    break;
                }
            }

            return res;
        }

        private GEDCOMFamilyRecord _ParseSource_GetParentsFamily(GEDCOMIndividualRecord iRec)
        {
            GEDCOMFamilyRecord result;
            if (iRec.ChildToFamilyLinks.Count > 0)
            {
                result = iRec.ChildToFamilyLinks[0].Family;
            }
            else
            {
                result = fBase.Tree.CreateFamily();
                result.AddChild(iRec);
            }
            return result;
        }

        private GEDCOMFamilyRecord _ParseSource_GetMarriageFamily(GEDCOMIndividualRecord iRec)
        {
            GEDCOMFamilyRecord result;

            if (iRec.SpouseToFamilyLinks.Count > 0)
            {
                result = iRec.SpouseToFamilyLinks[0].Family;
            }
            else
            {
                result = fBase.Tree.CreateFamily();
                result.AddSpouse(iRec);
            }

            return result;
        }

        private void CheckMain(GEDCOMIndividualRecord main)
        {
            if (main == null)
            {
                throw new PersonScanException(fLangMan.LS(FLS.LSID_BasePersonInvalid));
            }
        }

        public DataGridViewColumn AddTextColumn(string colName, string headerText)
        {
            DataGridViewColumn col = new DataGridViewTextBoxColumn();
            col.HeaderText = headerText;
            col.Name = colName;
            return col;
        }

        public DataGridViewColumn AddComboColumn(string colName, string headerText, object[] items)
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
            this.EditName.Text = "";
            this.EditBirthDate.Text = "";
            this.EditBirthPlace.Text = "";
            this.CheckBirth.Checked = false;
            this.EditDeathDate.Text = "";
            this.EditDeathPlace.Text = "";
            this.CheckDeath.Checked = false;
            this.MemoNote.Text = "";

            this.fSimpleTempSex = GEDCOMSex.svMale;
            this.btnMale.Text = new string(this.fLangMan.LS(FLS.LSID_SexM)[0], 1);
        }

        private void InitSourceControls()
        {
            this.fBase.Context.GetSourcesList(this.fSourcesList);

            cbSource.Items.Clear();

            int num = fSourcesList.Count;
            for (int i = 0; i < num; i++) {
                cbSource.Items.Add(fSourcesList[i]);
            }

            this.edPage.Text = "";
            this.edSourceYear.Text = "";
            this.edPlace.Text = "";
            this.edEventDate.Text = "";
            this.cbEventType.SelectedIndex = -1;
            this.dataGridView1.Rows.Clear();
        }

        private void InitGrid(DataGridView dgv)
        {
            int num = PersonLinks.Length;
            object[] linksList = new object[num];
            for (int i = 0; i < num; i++) linksList[i] = this.fLangMan.LS(PersonLinks[i]);

            ((System.ComponentModel.ISupportInitialize)(dgv)).BeginInit();
            dgv.Columns.AddRange(new DataGridViewColumn[] {
                                     AddComboColumn("FLink", this.fLangMan.LS(FLS.LSID_Join), linksList),
                                     AddTextColumn("FName", this.fLangMan.LS(FLS.LSID_Name)),
                                     AddTextColumn("FPatronymic", this.fLangMan.LS(FLS.LSID_Patronymic)),
                                     AddTextColumn("FSurname", this.fLangMan.LS(FLS.LSID_Surname)),
                                     AddTextColumn("FAge", this.fLangMan.LS(FLS.LSID_Age)),
                                     AddTextColumn("FComment", this.fLangMan.LS(FLS.LSID_Comment))});
            ((System.ComponentModel.ISupportInitialize)(dgv)).EndInit();
        }

        #endregion
        
        #region Parse functions
        
        private void ParseSimple()
        {
            string tmp = this.EditName.Text.ToLower();
            string[] tokens = tmp.Split(' ');
            if (tokens.Length < 3)
            {
                this.ShowError(fLangMan.LS(FLS.LSID_NameInvalid));
            }
            else
            {
                string fam = GEDCOMUtils.NormalizeName(tokens[0]);
                string nam = GEDCOMUtils.NormalizeName(tokens[1]);
                string pat = GEDCOMUtils.NormalizeName(tokens[2]);

                GEDCOMIndividualRecord iRec = this.fBase.Context.CreatePersonEx(nam, pat, fam, fSimpleTempSex, false);
                if (this.CheckBirth.Checked) {
                    this.fBase.Context.CreateEventEx(iRec, "BIRT", GEDCOMUtils.StrToGEDCOMDate(this.EditBirthDate.Text, true), this.EditBirthPlace.Text);
                }

                if (this.CheckDeath.Checked) {
                    this.fBase.Context.CreateEventEx(iRec, "DEAT", GEDCOMUtils.StrToGEDCOMDate(this.EditDeathDate.Text, true), this.EditDeathPlace.Text);
                }

                if (!string.IsNullOrEmpty(this.MemoNote.Text)) {
                    GEDCOMNoteRecord noteRec = fBase.Tree.CreateNote();
                    noteRec.SetNoteText(MemoNote.Text);
                    iRec.AddNote(noteRec);
                }

                this.fBase.ChangeRecord(iRec);

                this.InitSimpleControls();
            }
        }

        private void ParseSource()
        {
            string srcName = this.cbSource.Text;
            string srcPage = this.edPage.Text;
            int srcYear;
            string place = this.edPlace.Text;

            if (!int.TryParse(this.edSourceYear.Text, out srcYear))
            {
                this.ShowError(fLangMan.LS(FLS.LSID_SourceYearInvalid));
            }
            else
            {
                GEDCOMIndividualRecord iMain = null;
                try
                {
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

                            GEDCOMSex sx = this.fBase.DefineSex(nm, pt);
                            GEDCOMIndividualRecord iRec = this.fBase.Context.CreatePersonEx(nm, pt, fm, sx, false);

                            if (!string.IsNullOrEmpty(age) && GEDCOMUtils.IsDigits(age)) {
                                int birthYear = srcYear - int.Parse(age);
                                this.fBase.Context.CreateEventEx(iRec, "BIRT", "ABT "+birthYear.ToString(), "");
                            }

                            if (!string.IsNullOrEmpty(place)) {
                                GEDCOMCustomEvent evt = this.fBase.Context.CreateEventEx(iRec, "RESI", "", "");
                                evt.Detail.Place.StringValue = place;
                            }

                            if (!string.IsNullOrEmpty(comment)) {
                                GEDCOMNoteRecord noteRec = fBase.Tree.CreateNote();
                                noteRec.SetNoteText(comment);
                                iRec.AddNote(noteRec);
                            }

                            if (!string.IsNullOrEmpty(srcName)) {
                                GEDCOMSourceRecord srcRec = this.fBase.Context.FindSource(srcName);
                                if (srcRec == null) {
                                    srcRec = fBase.Tree.CreateSource();
                                    srcRec.FiledByEntry = srcName;
                                }
                                iRec.AddSource(srcRec, srcPage, 0);
                            }

                            fBase.ChangeRecord(iRec);

                            GEDCOMFamilyRecord family = null;

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
                                            GEDCOMCustomEvent evt = this.fBase.Context.CreateEventEx(iRec, evName, GEDCOMUtils.StrToGEDCOMDate(edEventDate.Text, false), "");
                                            evt.Detail.Place.StringValue = place;
                                        } else if (evName == "MARR") {
                                            family = _ParseSource_GetMarriageFamily(iRec);
                                            GEDCOMCustomEvent evt = this.fBase.Context.CreateEventEx(family, evName, GEDCOMUtils.StrToGEDCOMDate(edEventDate.Text, false), "");
                                            evt.Detail.Place.StringValue = place;
                                        }
                                    }
                                    break;

                                case PersonLink.plFather:
                                case PersonLink.plMother:
                                    CheckMain(iMain);
                                    family = _ParseSource_GetParentsFamily(iMain);
                                    family.AddSpouse(iRec);
                                    break;

                                case PersonLink.plGodparent:
                                    CheckMain(iMain);
                                    iMain.AddAssociation(fLangMan.LS(FLS.LSID_PLGodparent), iRec);
                                    break;

                                case PersonLink.plSpouse:
                                    CheckMain(iMain);
                                    family = _ParseSource_GetMarriageFamily(iMain);
                                    family.AddSpouse(iRec);
                                    break;

                                case PersonLink.plChild:
                                    CheckMain(iMain);
                                    family = _ParseSource_GetMarriageFamily(iMain);
                                    family.AddChild(iRec);
                                    break;
                            }
                        }
                    }
                }
                finally
                {
                }

                this.InitSourceControls();
            }
        }

        #endregion
        
        #region Form handlers
        
        private void btnParseClick(object sender, EventArgs e)
        {
            switch (this.PageControl1.SelectedIndex) {
                case 0:
                    this.ParseSimple();
                    break;
                case 1:
                    this.ParseSource();
                    break;
            }

            this.fBase.RefreshLists(false);
        }

        void BtnMaleClick(object sender, EventArgs e)
        {
            switch (fSimpleTempSex) {
                case GEDCOMSex.svMale:
                    this.btnMale.Text = new string(fLangMan.LS(FLS.LSID_SexF)[0], 1);
                    fSimpleTempSex = GEDCOMSex.svFemale;
                    break;
                case GEDCOMSex.svFemale:
                    this.btnMale.Text = new string(fLangMan.LS(FLS.LSID_SexM)[0], 1);
                    fSimpleTempSex = GEDCOMSex.svMale;
                    break;
            }
        }

        void EditBirthDateTextChanged(object sender, EventArgs e)
        {
            this.CheckBirth.Checked = true;
        }

        void EditDeathDateTextChanged(object sender, EventArgs e)
        {
            this.CheckDeath.Checked = true;
        }

        void RadioButton1CheckedChanged(object sender, EventArgs e)
        {
            gbMetrics.Enabled = (rbSK_Met.Checked);
        }

        #endregion
    }
}
