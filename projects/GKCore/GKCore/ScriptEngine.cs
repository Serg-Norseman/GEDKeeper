/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

#pragma warning disable IDE0060 // Remove unused parameter
#pragma warning disable IDE1006 // Naming Styles

#if !MOBILE

using System;
using System.Data;
using System.IO;
using System.Reflection;
using BSLib;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Controllers;
using GKCore.Design.Views;
using GKCore.Export;
using GKCore.Interfaces;
using GKCore.Options;
using NLua;
#if NETCORE
using System.Text;
#endif

namespace GKCore
{
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
        private IBaseWindow fBase;
        private readonly IScriptConsole fView;

        public ScriptEngine(IScriptConsole view)
        {
            fView = view;
        }

        public void lua_run(string script, IBaseWindow baseWin)
        {
            fBase = baseWin;

            using (Lua lvm = new Lua()) {
                try {
#if NETCORE
                    lvm.State.Encoding = Encoding.UTF8;
#endif
                    lua_init(lvm);
                    lvm.DoString(script);
                } catch (Exception ex) {
                    print("> " + LangMan.LS(LSID.Error) + ": " + ex.Message);
                }
            }
        }

        #region Private service functions

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
            lua_register(lvm, nameof(print));
            lua_register(lvm, nameof(progress_init));
            lua_register(lvm, nameof(progress_done));
            lua_register(lvm, nameof(progress_step));

            lua_register(lvm, nameof(strpos));
            lua_register(lvm, nameof(update_view));
            lua_register(lvm, nameof(select_file));
            lua_register(lvm, nameof(select_new_file));

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

            // any records
            lua_register(lvm, nameof(get_records_count));
            lua_register(lvm, nameof(get_record));
            lua_register(lvm, nameof(get_record_type));
            lua_register(lvm, nameof(get_record_type_name));
            lua_register(lvm, nameof(get_record_xref));
            lua_register(lvm, nameof(get_record_uid));
            lua_register(lvm, nameof(delete_record));
            lua_register(lvm, nameof(record_is_filtered));
            lua_register(lvm, nameof(select_record));

            // any record : notes
            lua_register(lvm, nameof(get_record_notes_count));

            // any record : multimedia links
            lua_register(lvm, nameof(get_record_medialinks_count));
            lua_register(lvm, nameof(get_record_medialink));
            lua_register(lvm, nameof(set_medialink_primary));

            // any record : bind notes, sources, multimedia
            lua_register(lvm, nameof(bind_record_note));
            lua_register(lvm, nameof(bind_record_source));

            // individual records
            lua_register(lvm, nameof(create_individual));
            lua_register(lvm, nameof(get_individual_name));
            lua_register(lvm, nameof(define_sex));
            lua_register(lvm, nameof(get_individual_sex));
            lua_register(lvm, nameof(set_individual_sex));
            lua_register(lvm, nameof(define_patronymic));
            lua_register(lvm, nameof(get_individual_parents_family));
            lua_register(lvm, nameof(get_individual_spouses_count));
            lua_register(lvm, nameof(get_individual_spouse_family));
            lua_register(lvm, nameof(get_individual_primary_medialink));

            // individual records : associations
            lua_register(lvm, nameof(get_individual_associations_count));
            lua_register(lvm, nameof(get_individual_association));
            lua_register(lvm, nameof(delete_individual_association));
            lua_register(lvm, nameof(add_individual_association));

            // individual records : events
            lua_register(lvm, nameof(get_individual_events_count));
            lua_register(lvm, nameof(get_individual_event));
            lua_register(lvm, nameof(delete_individual_event));
            lua_register(lvm, nameof(get_individual_event_ex));

            // individual records : groups
            lua_register(lvm, nameof(get_individual_groups_count));
            lua_register(lvm, nameof(get_individual_group));

            // family records
            lua_register(lvm, nameof(create_family));
            lua_register(lvm, nameof(bind_family_spouse));
            lua_register(lvm, nameof(bind_family_child));
            lua_register(lvm, nameof(get_family_husband));
            lua_register(lvm, nameof(get_family_wife));
            lua_register(lvm, nameof(get_family_childs_count));
            lua_register(lvm, nameof(get_family_child));

            // note records
            lua_register(lvm, nameof(create_note));
            lua_register(lvm, nameof(add_note_text));

            // any record with events (individual / family)
            lua_register(lvm, nameof(create_event));

            // events
            lua_register(lvm, nameof(get_event_value));
            lua_register(lvm, nameof(get_event_place));
            lua_register(lvm, nameof(get_event_date));
            lua_register(lvm, nameof(get_event_name));
            lua_register(lvm, nameof(set_event_value));
            lua_register(lvm, nameof(set_event_place));
            lua_register(lvm, nameof(set_event_date));
            lua_register(lvm, nameof(get_event_year));

            // source records
            lua_register(lvm, nameof(create_source));
            lua_register(lvm, nameof(find_source));

            // group records
            lua_register(lvm, nameof(create_group));
            lua_register(lvm, nameof(bind_group_member));
            lua_register(lvm, nameof(get_group_name));

            // location records
            lua_register(lvm, nameof(create_location));
            lua_register(lvm, nameof(get_location_usages));

            // csv
            lua_register(lvm, nameof(csv_load));
            lua_register(lvm, nameof(csv_close));
            lua_register(lvm, nameof(csv_get_cols));
            lua_register(lvm, nameof(csv_get_rows));
            lua_register(lvm, nameof(csv_get_cell));
            lua_register(lvm, nameof(csv_create));
            lua_register(lvm, nameof(csv_write_cell));

            // experimental
            lua_register(lvm, nameof(ado_open));
            lua_register(lvm, nameof(ado_close));
            lua_register(lvm, nameof(ado_query_open));
            lua_register(lvm, nameof(ado_query_close));
            lua_register(lvm, nameof(ado_query_first));
            lua_register(lvm, nameof(ado_query_prev));
            lua_register(lvm, nameof(ado_query_next));
            lua_register(lvm, nameof(ado_query_last));
            lua_register(lvm, nameof(ado_get_query_field));
            lua_register(lvm, nameof(ado_dump));
        }

        #endregion

        #region Misc functions

        public void print(object text)
        {
            if (fView != null && text != null)
                fView.print(text.ToString());
        }

        public int strpos(string substr, string str)
        {
            return str.IndexOf(substr);
        }

        #endregion

        #region UI functions

        public void progress_init(int length, string title)
        {
            // FIXME!
            //AppHost.Progress.ProgressInit(title, length);
        }

        public void progress_done()
        {
            // FIXME!
            //AppHost.Progress.ProgressDone();
        }

        public void progress_step()
        {
            // FIXME!
            //AppHost.Progress.ProgressStep();
        }

        public void update_view()
        {
            fBase.RefreshLists(false);
        }

        public string select_file()
        {
            return AppHost.StdDialogs.GetOpenFile("", "", "All files (*.*)|*.*", 0, "").Result;
        }

        public string select_new_file()
        {
            return AppHost.StdDialogs.GetSaveFile("", "", "All files (*.*)|*.*", 0, "", "", true).Result;
        }

        #endregion

        #region GEDCOM functions

        public int get_records_count()
        {
            return fBase.Context.Tree.RecordsCount;
        }

        public object get_record(int idx)
        {
            return fBase.Context.Tree[idx];
        }

        public int get_record_type(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return (rec == null) ? (int)GDMRecordType.rtNone : (int)rec.RecordType;
        }

        public bool delete_record(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return BaseController.DeleteRecord(fBase, rec, false).Result;
        }

        public string get_record_xref(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return (rec == null) ? string.Empty : rec.XRef;
        }

        public string get_record_uid(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return (rec == null) ? string.Empty : rec.UID;
        }

        public string get_record_type_name(int recType)
        {
            return ((GDMRecordType)recType).ToString();
        }

        public bool record_is_filtered(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return fBase.RecordIsFiltered(rec);
        }

        public object select_record(int recType)
        {
            return fBase.Context.SelectRecord(fView, (GDMRecordType)recType, null);
        }

        public string get_individual_name(object recPtr)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            return (iRec == null) ? "" : GKUtils.GetNameString(iRec, false);
        }

        public int get_individual_associations_count(object recPtr)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            return (iRec == null || !iRec.HasAssociations) ? 0 : iRec.Associations.Count;
        }

        public object get_individual_association(object recPtr, int idx)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            return (iRec == null || !iRec.HasAssociations) ? null : iRec.Associations[idx];
        }

        public object add_individual_association(object recPtr, string rel, object assoPtr)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            if (iRec == null) return null;

            GDMIndividualRecord assoRec = assoPtr as GDMIndividualRecord;
            if (assoRec == null) return null;

            return iRec.AddAssociation(rel, assoRec);
        }

        public void delete_individual_association(object recPtr, int idx)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            if (iRec == null || !iRec.HasAssociations) return;

            iRec.Associations.RemoveAt(idx);
        }

        public int get_individual_events_count(object recPtr)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            return (iRec == null) ? 0 : iRec.Events.Count;
        }

        public object get_individual_event(object recPtr, int idx)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            return (iRec == null) ? null : iRec.Events[idx];
        }

        public object get_individual_event_ex(object recPtr, string sign)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            if (iRec == null) return null;

            return iRec.FindEvent(sign);
        }

        public void delete_individual_event(object recPtr, int idx)
        {
            GDMIndividualRecord iRec = recPtr as GDMIndividualRecord;
            if (iRec == null) return;

            iRec.Events.RemoveAt(idx);
        }

        public string get_event_date(object evPtr)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            return (GKUtils.GEDCOMEventToDateStr(evt, GlobalOptions.Instance.DefDateFormat, false));
        }

        // TODO: checking this function, its incorrect logic
        public int get_event_year(object evPtr)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            if (evt == null) return 0;

            GDMDate date = evt.Date.Value as GDMDate;
            return (date == null) ? 0 : date.Year;
        }

        public void set_event_date(object evPtr, string date)
        {
            try {
                GDMCustomEvent evt = evPtr as GDMCustomEvent;
                if (evt != null && date != "") {
                    evt.Date.ParseString(date);
                }
            } catch {
                throw new ScriptException(LangMan.LS(LSID.DateFormatInvalid) + ": " + date);
            }
        }

        public string get_event_value(object evPtr)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            return (evt == null) ? string.Empty : evt.StringValue;
        }

        public string get_event_place(object evPtr)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            return (evt == null || !evt.HasPlace) ? string.Empty : evt.Place.StringValue;
        }

        public void set_event_value(object evPtr, string value)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            if (evt == null) return;

            evt.StringValue = value;
        }

        public void set_event_place(object evPtr, string place)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            if (evt == null) return;

            evt.Place.StringValue = place;
        }

        public string get_event_name(object evPtr)
        {
            GDMCustomEvent evt = evPtr as GDMCustomEvent;
            return (evt == null) ? string.Empty : evt.GetTagName();
        }

        public string get_individual_sex(object recPtr)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            return (rec == null) ? string.Empty : GKData.SexData[(int)rec.Sex].Sign;
        }

        public void set_individual_sex(object recPtr, string strSex)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            if (rec == null) return;

            GDMSex sex = (strSex.Length == 1) ? GKUtils.GetSexBySign(strSex[0]) : GDMSex.svUnknown;
            rec.Sex = sex;
        }

        public object create_individual(string name, string patronymic, string surname, string strSex)
        {
            GDMSex sex = (strSex.Length == 1) ? GKUtils.GetSexBySign(strSex[0]) : GDMSex.svUnknown;

            GDMIndividualRecord iRec = fBase.Context.CreatePersonEx(name, patronymic, surname, sex, false);
            return iRec;
        }

        public object create_family()
        {
            GDMFamilyRecord fRec = fBase.Context.Tree.CreateFamily();
            return fRec;
        }

        public object create_note()
        {
            GDMNoteRecord nRec = fBase.Context.Tree.CreateNote();
            return nRec;
        }

        public object create_source(string name)
        {
            GDMSourceRecord srcRec = fBase.Context.Tree.CreateSource();
            srcRec.ShortTitle = name;
            return srcRec;
        }

        public object create_group(string name)
        {
            GDMGroupRecord grpRec = fBase.Context.Tree.CreateGroup();
            grpRec.GroupName = name;
            return grpRec;
        }

        public void bind_group_member(object groupPtr, object indiPtr)
        {
            GDMGroupRecord grp = groupPtr as GDMGroupRecord;
            if (grp == null) return;

            GDMIndividualRecord indiRec = indiPtr as GDMIndividualRecord;
            grp.AddMember(indiRec);
        }

        public void add_note_text(object notePtr, string txt)
        {
            GDMNoteRecord nRec = notePtr as GDMNoteRecord;
            if (nRec == null) return;

            nRec.AddNoteText(txt);
        }

        public void bind_record_note(object recPtr, object notePtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            if (rec == null) return;

            GDMNoteRecord noteRec = notePtr as GDMNoteRecord;
            rec.AddNote(noteRec);
        }

        public void bind_record_source(object recPtr, object srcPtr, string page, int quality)
        {
            GDMRecord rec = recPtr as GDMRecord;
            if (rec == null) return;

            GDMSourceRecord srcRec = srcPtr as GDMSourceRecord;
            rec.AddSource(srcRec, page, quality);
        }

        public void bind_family_spouse(object familyPtr, object spousePtr)
        {
            GDMFamilyRecord fRec = familyPtr as GDMFamilyRecord;
            if (fRec == null) return;

            GDMIndividualRecord spRec = spousePtr as GDMIndividualRecord;
            fRec.AddSpouse(spRec);
        }

        public void bind_family_child(object familyPtr, object childPtr)
        {
            GDMFamilyRecord fRec = familyPtr as GDMFamilyRecord;
            if (fRec == null) return;

            GDMIndividualRecord chRec = childPtr as GDMIndividualRecord;
            fRec.AddChild(chRec);
        }

        public string define_sex(string name, string patr)
        {
            GDMSex sx = fBase.Context.DefineSex(fView, name, patr).Result;

            return GKData.SexData[(int)sx].Sign;
        }

        public object find_source(string name)
        {
            return fBase.Context.FindSource(name);
        }

        public object create_event(object recPtr, string sign)
        {
            GDMRecordWithEvents rec = recPtr as GDMRecordWithEvents;
            GDMCustomEvent evt = fBase.Context.CreateEventEx(rec, sign, "", "");
            return evt;
        }

        public string define_patronymic(string fatherName, string childSex, bool confirm)
        {
            GDMSex sex = (childSex.Length == 1) ? GKUtils.GetSexBySign(childSex[0]) : GDMSex.svUnknown;

            return fBase.Context.DefinePatronymic(fView, fatherName, sex, confirm).Result;
        }

        public object get_individual_parents_family(object recPtr)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            return (rec == null) ? null : fBase.Context.Tree.GetParentsFamily(rec);
        }

        public int get_individual_spouses_count(object recPtr)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            return (rec == null) ? 0 : rec.SpouseToFamilyLinks.Count;
        }

        public object get_individual_spouse_family(object recPtr, int spIdx)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            return (rec == null) ? null : fBase.Context.Tree.GetPtrValue(rec.SpouseToFamilyLinks[spIdx]);
        }

        public object get_family_husband(object recPtr)
        {
            GDMFamilyRecord fam = recPtr as GDMFamilyRecord;
            return (fam == null) ? null : fBase.Context.Tree.GetPtrValue<GDMRecord>(fam.Husband);
        }

        public object get_family_wife(object recPtr)
        {
            GDMFamilyRecord fam = recPtr as GDMFamilyRecord;
            return (fam == null) ? null : fBase.Context.Tree.GetPtrValue<GDMRecord>(fam.Wife);
        }

        public int get_family_childs_count(object recPtr)
        {
            GDMFamilyRecord fam = recPtr as GDMFamilyRecord;
            return (fam == null) ? -1 : fam.Children.Count;
        }

        public object get_family_child(object recPtr, int childIndex)
        {
            GDMFamilyRecord fam = recPtr as GDMFamilyRecord;
            return (fam == null) ? null : fBase.Context.Tree.GetPtrValue<GDMRecord>(fam.Children[childIndex]);
        }

        public object create_location(string name, string date)
        {
            var loc = fBase.Context.Tree.CreateLocation();
            var locName = new GDMLocationName() {
                StringValue = name,
                Date = { StringValue = date }
            };
            loc.Names.Add(locName);
            return loc;
        }

        public int get_location_usages(object recPtr)
        {
            GDMLocationRecord loc = recPtr as GDMLocationRecord;
            if (loc == null) return -1;

            int usages;

            StringList linkList = null;
            try {
                linkList = GKUtils.GetLocationLinks(fBase.Context.Tree, loc);
                usages = linkList.Count;
            } finally {
                if (linkList != null) linkList.Dispose();
            }

            return usages;
        }

        public int get_record_notes_count(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return (rec == null) ? -1 : rec.Notes.Count;
        }

        public int get_record_medialinks_count(object recPtr)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return (rec == null || !rec.HasMultimediaLinks) ? -1 : rec.MultimediaLinks.Count;
        }

        public object get_record_medialink(object recPtr, int index)
        {
            GDMRecord rec = recPtr as GDMRecord;
            return (rec == null || !rec.HasMultimediaLinks) ? null : rec.MultimediaLinks[index];
        }

        public void set_medialink_primary(object linkPtr, bool value)
        {
            var link = linkPtr as GDMMultimediaLink;
            if (link != null) {
                link.IsPrimary = value;
            }
        }

        public object get_individual_primary_medialink(object indiPtr)
        {
            var indiRec = indiPtr as GDMIndividualRecord;
            return (indiRec == null || !indiRec.HasMultimediaLinks) ? null : indiRec.GetPrimaryMultimediaLink();
        }

        public int get_individual_groups_count(object recPtr)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            return (rec == null || !rec.HasGroups) ? 0 : rec.Groups.Count;
        }

        public object get_individual_group(object recPtr, int grIdx)
        {
            GDMIndividualRecord rec = recPtr as GDMIndividualRecord;
            return (rec == null) ? null : fBase.Context.Tree.GetPtrValue<GDMGroupRecord>(rec.Groups[grIdx]);
        }

        public string get_group_name(object recPtr)
        {
            GDMGroupRecord grp = recPtr as GDMGroupRecord;
            return (grp == null) ? string.Empty : grp.GroupName;
        }

        #endregion

        #region CSV functions

        private DataTable fCSVData = null;
        private CSVWriter fCSVWriter = null;

        public bool csv_load(string fileName, bool hasHeader)
        {
            bool result = false;
            if (!File.Exists(fileName)) return result;

            try {
                fCSVData = CSVReader.ReadCSVFile(fileName, GKUtils.DetectEncoding(fileName), hasHeader);
                result = true;
            } catch (Exception e) {
                Logger.WriteError("ScriptEngine.csv_load(" + fileName + "): fail", e);

                result = false;
            }

            return result;
        }

        public bool csv_create(string fileName, int columnsCount, int rowsCount)
        {
            bool result;

            try {
                fCSVWriter = new CSVWriter();
                fCSVWriter.SetFileName(fileName);
                fCSVWriter.BeginWrite();
                fCSVWriter.BeginTable(columnsCount, rowsCount);
                result = true;
            } catch {
                result = false;
            }

            return result;
        }

        public void csv_close()
        {
            try {
                if (fCSVData != null) {
                    fCSVData.Dispose();
                }

                if (fCSVWriter != null) {
                    fCSVWriter.EndTable();
                    fCSVWriter.EndWrite();
                    fCSVWriter.Dispose();
                }
            } catch (Exception ex) {
                throw new ScriptException(ex.Message);
            }
        }

        public int csv_get_cols()
        {
            if (fCSVData == null) return 0;

            return fCSVData.Columns.Count;
        }

        public int csv_get_rows()
        {
            if (fCSVData == null) return 0;

            return fCSVData.Rows.Count;
        }

        public string csv_get_cell(int col, int row)
        {
            if (fCSVData == null) return string.Empty;

            DataRow dr = fCSVData.Rows[row];
            return dr.ItemArray[col].ToString();
        }

        public void csv_write_cell(object content)
        {
            if (fCSVWriter == null) return;

            string strContent = (content != null) ? content.ToString() : string.Empty;
            fCSVWriter.AddTableCell(strContent);
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

#endif
