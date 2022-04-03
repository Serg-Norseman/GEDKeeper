/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

#if MONO
#define NLUA
#endif

using System;
using System.Data;
using System.IO;
using System.Reflection;
using System.Text;
using BSLib;
using BSLib.Design.MVP.Controls;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Controllers;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKCore
{
    #if !NLUA
    using LuaInterface;
    #else
    using NLua;
    #endif

    public class ScriptException : Exception
    {
        public ScriptException()
        {
        }

        public ScriptException(string message) : base(message)
        {
        }
    }

    /// <summary>
    ///
    /// </summary>
    public class ScriptEngine
    {
        private ITextBox fDebugOutput;
        private IBaseWindow fBase;

        public void lua_run(string script, IBaseWindow baseWin, ITextBox debugOutput)
        {
            fDebugOutput = debugOutput;
            fBase = baseWin;

            using (Lua lvm = new Lua())
            {
                try {
                    lua_init(lvm);
                    lvm.DoString(script);
                } catch (Exception ex) {
                    lua_print("> "+LangMan.LS(LSID.LSID_Error)+": " + ex.Message);
                }
            }
        }

        #region Private service functions

        private void lua_print(object text)
        {
            if (fDebugOutput == null) return;

            fDebugOutput.AppendText(text + @"\r\n");
        }

        private void lua_register(Lua lvm, string funcName)
        {
            MethodInfo mInfo = GetType().GetMethod(funcName);
            if (mInfo != null) {
                lvm.RegisterFunction(funcName, this, mInfo);
            } else {
                Logger.WriteError("ScriptEngine.lua_register(" + funcName + "): fail");
            }
        }

        private void lua_init(Lua lvm)
        {
            lua_register(lvm, "gk_print");
            lua_register(lvm, "gk_progress_init");
            lua_register(lvm, "gk_progress_done");
            lua_register(lvm, "gk_progress_step");

            lua_register(lvm, "gk_strpos");
            lua_register(lvm, "gk_update_view");
            lua_register(lvm, "gk_select_file");

            lvm["rtNone"] = (int)GDMRecordType.rtNone;
            lvm["rtIndividual"] = (int)GDMRecordType.rtIndividual;
            lvm["rtFamily"] = (int)GDMRecordType.rtFamily;
            lvm["rtNote"] = (int)GDMRecordType.rtNote;
            lvm["rtMultimedia"] = (int)GDMRecordType.rtMultimedia;
            lvm["rtSource"] = (int)GDMRecordType.rtSource;
            lvm["rtRepository"] = (int)GDMRecordType.rtRepository;
            lvm["rtGroup"] = (int)GDMRecordType.rtGroup;
            lvm["rtResearch"] = (int)GDMRecordType.rtResearch;
            lvm["rtTask"] = (int)GDMRecordType.rtTask;
            lvm["rtCommunication"] = (int)GDMRecordType.rtCommunication;
            lvm["rtLocation"] = (int)GDMRecordType.rtLocation;
            lvm["rtSubmission"] = (int)GDMRecordType.rtSubmission;
            lvm["rtSubmitter"] = (int)GDMRecordType.rtSubmitter;

            lua_register(lvm, "gt_get_records_count");
            lua_register(lvm, "gt_get_record");
            lua_register(lvm, "gt_get_record_type");
            lua_register(lvm, "gt_get_record_type_name");
            lua_register(lvm, "gt_get_record_xref");
            lua_register(lvm, "gt_get_record_uid");

            lua_register(lvm, "gt_delete_record");
            lua_register(lvm, "gt_record_is_filtered");
            lua_register(lvm, "gt_select_record");

            lua_register(lvm, "gt_create_person");
            lua_register(lvm, "gt_create_family");
            lua_register(lvm, "gt_create_note");

            lua_register(lvm, "gt_get_person_name");
            lua_register(lvm, "gt_define_sex");

            lua_register(lvm, "gt_get_person_associations_count");
            lua_register(lvm, "gt_get_person_association");
            lua_register(lvm, "gt_delete_person_association");

            lua_register(lvm, "gt_get_person_events_count");
            lua_register(lvm, "gt_get_person_event");
            lua_register(lvm, "gt_delete_person_event");

            lua_register(lvm, "gt_bind_record_note");
            lua_register(lvm, "gt_bind_record_source");

            lua_register(lvm, "gt_bind_family_spouse");
            lua_register(lvm, "gt_bind_family_child");

            lua_register(lvm, "gt_add_note_text");

            lua_register(lvm, "gt_create_event");

            lua_register(lvm, "gt_get_event_value");
            lua_register(lvm, "gt_get_event_place");
            lua_register(lvm, "gt_get_event_date");
            lua_register(lvm, "gt_get_event_name");

            lua_register(lvm, "gt_set_event_value");
            lua_register(lvm, "gt_set_event_place");
            lua_register(lvm, "gt_set_event_date");

            lua_register(lvm, "gt_create_source");
            lua_register(lvm, "gt_find_source");

            lua_register(lvm, "gt_create_group");
            lua_register(lvm, "gt_bind_group_member");

            //

            lua_register(lvm, "gt_get_person_event_ex");
            lua_register(lvm, "gt_get_event_year");

            //

            lua_register(lvm, "csv_load");
            lua_register(lvm, "csv_close");
            lua_register(lvm, "csv_get_cols");
            lua_register(lvm, "csv_get_rows");
            lua_register(lvm, "csv_get_cell");

            //

            lua_register(lvm, "gt_add_person_association");
            lua_register(lvm, "gt_define_patronymic");
            lua_register(lvm, "gt_get_person_parents_family");
            lua_register(lvm, "gt_get_person_spouses_count");
            lua_register(lvm, "gt_get_person_spouse_family");
            lua_register(lvm, "gt_get_family_husband");
            lua_register(lvm, "gt_get_family_wife");
            lua_register(lvm, "gt_get_family_childs_count");
            lua_register(lvm, "gt_get_family_child");

            lua_register(lvm, "gt_get_location_usages");
            lua_register(lvm, "gt_get_record_notes_count");
            lua_register(lvm, "gt_get_person_sex");
            lua_register(lvm, "gt_set_person_sex");

            lua_register(lvm, "gt_get_person_groups_count");
            lua_register(lvm, "gt_get_person_group");
            lua_register(lvm, "gt_get_group_name");

            // experimental

            lua_register(lvm, "ado_open");
            lua_register(lvm, "ado_close");
            lua_register(lvm, "ado_query_open");
            lua_register(lvm, "ado_query_close");
            lua_register(lvm, "ado_query_first");
            lua_register(lvm, "ado_query_prev");
            lua_register(lvm, "ado_query_next");
            lua_register(lvm, "ado_query_last");
            lua_register(lvm, "ado_get_query_field");
            lua_register(lvm, "ado_dump");
        }

        #endregion

        #region Misc functions

        public void gk_print(object text)
        {
            lua_print(text);
        }

        public int gk_strpos(string substr, string str)
        {
            return str.IndexOf(substr);
        }

        #endregion

        #region UI functions

        public void gk_progress_init(int length, string title)
        {
            AppHost.Progress.ProgressInit(title, length);
        }

        public void gk_progress_done()
        {
            AppHost.Progress.ProgressDone();
        }

        public void gk_progress_step()
        {
            AppHost.Progress.ProgressStep();
        }

        public void gk_update_view()
        {
            fBase.RefreshLists(false);
        }

        public string gk_select_file()
        {
            string filename = AppHost.StdDialogs.GetOpenFile("", "", "All files (*.*)|*.*", 0, "");
            return filename;
        }

        #endregion

        #region GEDCOM functions

        public int gt_get_records_count()
        {
            return fBase.Context.Tree.RecordsCount;
        }

        public object gt_get_record(int idx)
        {
            return fBase.Context.Tree[idx];
        }

        public int gt_get_record_type(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return (rec == null) ? (int)GDMRecordType.rtNone : (int)rec.RecordType;
        }

        public bool gt_delete_record(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            bool res = BaseController.DeleteRecord(fBase, rec, false);
            return res;
        }

        public string gt_get_record_xref(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return (rec == null) ? string.Empty : rec.XRef;
        }

        public string gt_get_record_uid(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return (rec == null) ? string.Empty : rec.UID;
        }

        public string gt_get_record_type_name(int recType)
        {
            GDMRecordType rt = (GDMRecordType)recType;
            string rtName = rt.ToString();
            return rtName;
        }

        public bool gt_record_is_filtered(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return fBase.RecordIsFiltered(rec);
        }

        public object gt_select_record(int recType)
        {
            GDMRecord rec = fBase.Context.SelectRecord((GDMRecordType)recType, null);
            return rec;
        }

        public string gt_get_person_name(object recPtr)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            return ((iRec == null) ? "" : GKUtils.GetNameString(iRec, true, false));
        }

        public int gt_get_person_associations_count(object recPtr)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            return (iRec == null || !iRec.HasAssociations) ? 0 : iRec.Associations.Count;
        }

        public object gt_get_person_association(object recPtr, int idx)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            if (iRec == null || !iRec.HasAssociations) return null;

            GDMAssociation asso = iRec.Associations[idx];
            return asso;
        }

        public object gt_add_person_association(object recPtr, string rel, object assoPtr)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            if (iRec == null) return null;

            GDMIndividualRecord assoRec = assoPtr as GDMIndividualRecord;
            if (assoRec == null) return null;

            GDMAssociation asso = iRec.AddAssociation(rel, assoRec);
            return asso;
        }

        public void gt_delete_person_association(object recPtr, int idx)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            if (iRec == null || !iRec.HasAssociations) return;

            iRec.Associations.DeleteAt(idx);
        }

        public int gt_get_person_events_count(object recPtr)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            return (iRec == null) ? 0 : iRec.Events.Count;
        }

        public object gt_get_person_event(object recPtr, int idx)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            if (iRec == null) return null;

            GDMCustomEvent evt = iRec.Events[idx];
            return evt;
        }

        public object gt_get_person_event_ex(object recPtr, string sign)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            if (iRec == null) return null;

            GDMCustomEvent evt = iRec.FindEvent(sign);
            return evt;
        }

        public void gt_delete_person_event(object recPtr, int idx)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            if (iRec == null) return;

            iRec.Events.DeleteAt(idx);
        }

        public string gt_get_event_date(object evPtr)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            return (GKUtils.GEDCOMEventToDateStr(evt, GlobalOptions.Instance.DefDateFormat, false));
        }

        // TODO: checking this function, its incorrect logic
        public int gt_get_event_year(object evPtr)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            if (evt == null) return 0;

            GDMDate date = evt.Date.Value as GDMDate;
            return (date == null) ? 0 : date.Year;
        }

        public void gt_set_event_date(object evPtr, string date)
        {
            try
            {
                GDMCustomEvent evt = evPtr as GDMCustomEvent;
                if (evt != null && date != "")
                {
                    evt.Date.ParseString(date);
                }
            }
            catch
            {
                throw new ScriptException(LangMan.LS(LSID.LSID_DateFormatInvalid) + ": " + date);
            }
        }

        public string gt_get_event_value(object evPtr)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            return (evt == null) ? string.Empty : evt.StringValue;
        }

        public string gt_get_event_place(object evPtr)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            return (evt == null || !evt.HasPlace) ? string.Empty : evt.Place.StringValue;
        }

        public void gt_set_event_value(object evPtr, string value)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            if (evt == null) return;

            evt.StringValue = value;
        }

        public void gt_set_event_place(object evPtr, string place)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            if (evt == null) return;

            evt.Place.StringValue = place;
        }

        public string gt_get_event_name(object evPtr)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            return (evt == null) ? string.Empty : evt.GetTagName();
        }

        public string gt_get_person_sex(object recPtr)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            return (rec == null) ? string.Empty : GKData.SexData[(int)rec.Sex].Sign;
        }

        public void gt_set_person_sex(object recPtr, string strSex)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            if (rec == null) return;

            GDMSex sex = (strSex.Length == 1) ? GKUtils.GetSexBySign(strSex[0]) : GDMSex.svUnknown;
            rec.Sex = sex;
        }

        public object gt_create_person(string name, string patronymic, string surname, string strSex)
        {
            GDMSex sex = (strSex.Length == 1) ? GKUtils.GetSexBySign(strSex[0]) : GDMSex.svUnknown;

            GDMIndividualRecord iRec = fBase.Context.CreatePersonEx(name, patronymic, surname, sex, false);
            return iRec;
        }

        public object gt_create_family()
        {
            GDMFamilyRecord fRec = fBase.Context.Tree.CreateFamily();
            return fRec;
        }

        public object gt_create_note()
        {
            GDMNoteRecord nRec = fBase.Context.Tree.CreateNote();
            return nRec;
        }

        public object gt_create_source(string name)
        {
            GDMSourceRecord srcRec = fBase.Context.Tree.CreateSource();
            srcRec.ShortTitle = name;
            return srcRec;
        }

        public object gt_create_group(string name)
        {
            GDMGroupRecord grpRec = fBase.Context.Tree.CreateGroup();
            grpRec.GroupName = name;
            return grpRec;
        }

        public void gt_bind_group_member(object groupPtr, object personPtr)
        {
            GDMGroupRecord grp = groupPtr as GDMGroupRecord;
            if (grp == null) return;

            GDMIndividualRecord person = personPtr as GDMIndividualRecord;
            grp.AddMember(person);
        }

        public void gt_add_note_text(object notePtr, string txt)
        {
            GDMNoteRecord nRec = notePtr as GDMNoteRecord;
            if (nRec == null) return;

            nRec.AddNoteText(txt);
        }

        public void gt_bind_record_note(object recPtr, object notePtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            if (rec == null) return;

            GDMNoteRecord noteRec = notePtr as GDMNoteRecord;
            rec.AddNote(noteRec);
        }

        public void gt_bind_record_source(object recPtr, object srcPtr, string page, int quality)
        {
            GDMRecord rec = recPtr as GDMRecord;
            if (rec == null) return;

            GDMSourceRecord srcRec = srcPtr as GDMSourceRecord;
            rec.AddSource(srcRec, page, quality);
        }

        public void gt_bind_family_spouse(object familyPtr, object spousePtr)
        {
            GDMFamilyRecord fRec = familyPtr as GDMFamilyRecord;
            if (fRec == null) return;

            GDMIndividualRecord spRec = spousePtr as GDMIndividualRecord;
            fRec.AddSpouse(spRec);
        }

        public void gt_bind_family_child(object familyPtr, object childPtr)
        {
            GDMFamilyRecord fRec = familyPtr as GDMFamilyRecord;
            if (fRec == null) return;

            GDMIndividualRecord chRec = childPtr as GDMIndividualRecord;
            fRec.AddChild(chRec);
        }

        public string gt_define_sex(string name, string patr)
        {
            GDMSex sx = fBase.Context.DefineSex(name, patr);

            return (GKData.SexData[(int)sx].Sign);
        }

        public object gt_find_source(string name)
        {
            GDMSourceRecord srcRec = fBase.Context.FindSource(name);
            return srcRec;
        }

        public object gt_create_event(object recPtr, string sign)
        {
            GDMRecordWithEvents rec = recPtr as GDMRecordWithEvents;
            GDMCustomEvent evt = fBase.Context.CreateEventEx(rec, sign, "", "");

            return evt;
        }

        public string gt_define_patronymic(string fatherName, string childSex, bool confirm)
        {
            GDMSex sex = (childSex.Length == 1) ? GKUtils.GetSexBySign(childSex[0]) : GDMSex.svUnknown;

            string childPatronymic = fBase.Context.DefinePatronymic(fatherName, sex, confirm);
            return childPatronymic;
        }

        public object gt_get_person_parents_family(object recPtr)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            if (rec == null) return null;

            GDMFamilyRecord fam = fBase.Context.Tree.GetParentsFamily(rec);
            return fam;
        }

        public int gt_get_person_spouses_count(object recPtr)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            return (rec == null) ? 0 : rec.SpouseToFamilyLinks.Count;
        }

        public object gt_get_person_spouse_family(object recPtr, int spIdx)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            if (rec == null) return null;

            GDMFamilyRecord fam = fBase.Context.Tree.GetPtrValue(rec.SpouseToFamilyLinks[spIdx]);
            return fam;
        }

        public object gt_get_family_husband(object recPtr)
        {
            GDMFamilyRecord fam = recPtr as GDMFamilyRecord;
            recPtr = (fam == null) ? null : fBase.Context.Tree.GetPtrValue<GDMRecord>(fam.Husband);
            return recPtr;
        }

        public object gt_get_family_wife(object recPtr)
        {
            GDMFamilyRecord fam = recPtr as GDMFamilyRecord;
            recPtr = (fam == null) ? null : fBase.Context.Tree.GetPtrValue<GDMRecord>(fam.Wife);
            return recPtr;
        }

        public int gt_get_family_childs_count(object recPtr)
        {
            GDMFamilyRecord fam = recPtr as GDMFamilyRecord;
            return (fam == null) ? -1 : fam.Children.Count;
        }

        public object gt_get_family_child(object recPtr, int childIndex)
        {
            GDMFamilyRecord fam = recPtr as GDMFamilyRecord;
            return (fam == null) ? null : fBase.Context.Tree.GetPtrValue<GDMRecord>(fam.Children[childIndex]);
        }

        public int gt_get_location_usages(object recPtr)
        {
            GDMLocationRecord loc = recPtr as GDMLocationRecord;
            if (loc == null) return -1;

            int usages;

            StringList linkList = null;
            try
            {
                linkList = GKUtils.GetLocationLinks(fBase.Context.Tree, loc);
                usages = linkList.Count;
            }
            finally
            {
                if (linkList != null) linkList.Dispose();
            }

            return usages;
        }

        public int gt_get_record_notes_count(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return (rec == null) ? -1 : rec.Notes.Count;
        }

        public int gt_get_person_groups_count(object recPtr)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            return (rec == null || !rec.HasGroups) ? 0 : rec.Groups.Count;
        }

        public object gt_get_person_group(object recPtr, int grIdx)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            if (rec == null) return null;

            var grp = fBase.Context.Tree.GetPtrValue<GDMGroupRecord>(rec.Groups[grIdx]);
            return grp;
        }

        public string gt_get_group_name(object recPtr)
        {
            GDMGroupRecord grp = recPtr as GDMGroupRecord;
            return (grp == null) ? "" : grp.GroupName;
        }

        #endregion

        #region CSV functions

        private DataTable fCSVData = null;

        public bool csv_load(string fileName, bool hasHeader)
        {
            bool result = false;
            if (!File.Exists(fileName)) return result;

            try
            {
                fCSVData = CSVReader.ReadCSVFile(fileName, Encoding.Unicode, hasHeader);
                result = true;
            }
            catch
            {
                result = false;
            }

            return result;
        }

        public void csv_close()
        {
            try
            {
                fCSVData.Dispose();
            }
            catch (Exception ex)
            {
                throw new ScriptException(ex.Message);
            }
        }

        public int csv_get_cols()
        {
            return fCSVData.Columns.Count;
        }

        public int csv_get_rows()
        {
            return fCSVData.Rows.Count;
        }

        public string csv_get_cell(int col, int row)
        {
            DataRow dr = fCSVData.Rows[row];
            return dr.ItemArray[col].ToString();
        }

        #endregion

        #region ADO functions

        public object ado_open(string constr)
        {
            return null;
        }

        public bool ado_close(object conptr)
        {
            return false;
        }

        public object ado_query_open(object conptr, string query)
        {
            return null;
        }

        public bool ado_query_close(object qptr)
        {
            return false;
        }

        public bool ado_query_first(object qptr)
        {
            return false;
        }

        public bool ado_query_prev(object qptr)
        {
            return false;
        }

        public bool ado_query_next(object qptr)
        {
            return false;
        }

        public bool ado_query_last(object qptr)
        {
            return false;
        }

        public string ado_get_query_field(object qptr, string fname)
        {
            return "";
        }

        public void ado_dump(object conptr)
        {
        }

        #endregion
    }
}
