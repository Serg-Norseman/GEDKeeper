using System;
using System.Data;
using System.IO;
using System.Reflection;
using System.Windows.Forms;

using BSLib;
using Com.StellmanGreene.CSVReader;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;
using LuaInterface;

namespace GKCore
{
	[Serializable]
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
    public class ScriptEngine : BaseObject
	{
		private TextBox fDebugOutput;
		private IBaseWindow fBase;

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				// dummy
			}
			base.Dispose(disposing);
		}

		private void lua_print(string s)
		{
			fDebugOutput.Text += (s + @"\r\n");
		}

		private void lua_register(Lua lvm, string funcName)
		{
			MethodInfo mInfo = this.GetType().GetMethod(funcName);
			if (mInfo != null) {
				lvm.RegisterFunction(funcName, this, mInfo);
			}
			else
			{
				this.fBase.Host.LogWrite("ScriptEngine.lua_register(" + funcName + "): fail");
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

			lvm["rtNone"] = (int)GEDCOMRecordType.rtNone;
			lvm["rtIndividual"] = (int)GEDCOMRecordType.rtIndividual;
			lvm["rtFamily"] = (int)GEDCOMRecordType.rtFamily;
			lvm["rtNote"] = (int)GEDCOMRecordType.rtNote;
			lvm["rtMultimedia"] = (int)GEDCOMRecordType.rtMultimedia;
			lvm["rtSource"] = (int)GEDCOMRecordType.rtSource;
			lvm["rtRepository"] = (int)GEDCOMRecordType.rtRepository;
			lvm["rtGroup"] = (int)GEDCOMRecordType.rtGroup;
			lvm["rtResearch"] = (int)GEDCOMRecordType.rtResearch;
			lvm["rtTask"] = (int)GEDCOMRecordType.rtTask;
			lvm["rtCommunication"] = (int)GEDCOMRecordType.rtCommunication;
			lvm["rtLocation"] = (int)GEDCOMRecordType.rtLocation;
			lvm["rtSubmission"] = (int)GEDCOMRecordType.rtSubmission;
			lvm["rtSubmitter"] = (int)GEDCOMRecordType.rtSubmitter;

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

			// experimentals

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

		public void lua_run(string script, IBaseWindow aBase, TextBox aDebugOutput)
		{
			this.fDebugOutput = aDebugOutput;
            this.fBase = aBase;

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

		/////

		public void gk_print(string text)
		{
			lua_print(text);
		}

		public void gk_progress_init(int length, string title)
		{
			this.fBase.ProgressInit(title, length);
		}

		public void gk_progress_done()
		{
			this.fBase.ProgressDone();
		}

		public void gk_progress_step()
		{
			this.fBase.ProgressStep();
		}

		public int gk_strpos(string substr, string str)
		{
			return str.IndexOf(substr);
		}

		public void gk_update_view()
		{
			fBase.RefreshLists(false);
		}

		public string gk_select_file()
		{
			string fn;
			using (OpenFileDialog dlg = new OpenFileDialog()) {
				fn = (dlg.ShowDialog() == DialogResult.OK) ? dlg.FileName : "";
			}
			return fn;
		}

		public int gt_get_records_count()
		{
			return (fBase.Tree.RecordsCount);
		}

		public object gt_get_record(int idx)
		{
			return fBase.Tree[idx];
		}

		public int gt_get_record_type(object recPtr)
		{
			GEDCOMRecord rec = recPtr as GEDCOMRecord;
			return (rec == null) ? (int)GEDCOMRecordType.rtNone : (int)rec.RecordType;
		}

		public bool gt_delete_record(object recPtr)
		{
			GEDCOMRecord rec = recPtr as GEDCOMRecord;
			bool res = fBase.RecordDelete(rec, false);
			return res;
		}

		public string gt_get_record_xref(object recPtr)
		{
			GEDCOMRecord rec = recPtr as GEDCOMRecord;
			return (rec == null) ? string.Empty : rec.XRef;
		}

		public string gt_get_record_uid(object recPtr)
		{
			GEDCOMRecord rec = recPtr as GEDCOMRecord;
            return (rec == null) ? string.Empty : rec.UID;
		}

		public string gt_get_record_type_name(int recType)
		{
			GEDCOMRecordType rt = (GEDCOMRecordType)recType;
			string rtName = rt.ToString();
			return rtName;
		}

		public bool gt_record_is_filtered(object recPtr)
		{
			GEDCOMRecord rec = recPtr as GEDCOMRecord;
			return fBase.RecordIsFiltered(rec);
		}

		public object gt_select_record(int recType)
		{
			GEDCOMRecord rec = fBase.SelectRecord((GEDCOMRecordType)recType, null);
			return rec;
		}

		public string gt_get_person_name(object recPtr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
			return ((rec == null) ? "" : rec.GetNameString(true, false));
		}

		public int gt_get_person_associations_count(object recPtr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
            return (rec == null) ? 0 : rec.Associations.Count;
		}

		public object gt_get_person_association(object recPtr, int idx)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
		    if (rec == null) return null;

			GEDCOMAssociation asso = rec.Associations[idx];
			return asso;
		}

		public object gt_add_person_association(object recPtr, string rel, object assoPtr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
            if (rec == null) return null;

			GEDCOMIndividualRecord assoRec = assoPtr as GEDCOMIndividualRecord;
            if (assoRec == null) return null;

			GEDCOMAssociation asso = rec.AddAssociation(rel, assoRec);
			return asso;
		}

		public void gt_delete_person_association(object recPtr, int idx)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
            if (rec == null) return;

			rec.Associations.DeleteAt(idx);
		}

		public int gt_get_person_events_count(object recPtr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
            return (rec == null) ? 0 : rec.Events.Count;
		}

		public object gt_get_person_event(object recPtr, int idx)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
            if (rec == null) return null;

			GEDCOMCustomEvent evt = rec.Events[idx];
			return evt;
		}

		public object gt_get_person_event_ex(object recPtr, string sign)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
            if (rec == null) return null;

			GEDCOMCustomEvent evt = rec.FindEvent(sign);
			return evt;
		}

		public void gt_delete_person_event(object recPtr, int idx)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
            if (rec == null) return;

			rec.Events.DeleteAt(idx);
		}

		public string gt_get_event_date(object evPtr)
		{
			GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
			return (GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false));
		}

		public int gt_get_event_year(object evPtr)
		{
			GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
			if (evt == null) {
				return 0;
			}

			int year = GEDCOMUtils.GetAbstractDate(evt).Year;
			return year;
		}

		public void gt_set_event_date(object evPtr, string date)
		{
			try
			{
                GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
                if (evt != null && date != "")
                {
					evt.Detail.Date.ParseString(date);
				}
			}
			catch
			{
				throw new ScriptException(LangMan.LS(LSID.LSID_DateFormatInvalid) + ": " + date);
			}
		}

		public string gt_get_event_value(object evPtr)
		{
			GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
			return (evt == null) ? string.Empty : evt.StringValue;
		}

		public string gt_get_event_place(object evPtr)
		{
			GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
            return (evt == null) ? string.Empty : evt.Detail.Place.StringValue;
		}

		public void gt_set_event_value(object evPtr, string value)
		{
			GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
		    if (evt == null) return;

			evt.StringValue = value;
		}

		public void gt_set_event_place(object evPtr, string place)
		{
			GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
            if (evt == null) return;

			evt.Detail.Place.StringValue = place;
		}

		public string gt_get_event_name(object evPtr)
		{
			GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
            return (evt == null) ? string.Empty : evt.Name;
		}

		public string gt_get_person_sex(object recPtr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
            return (rec == null) ? string.Empty : GKData.SexData[(int)rec.Sex].Sign;
		}

		public void gt_set_person_sex(object recPtr, string strSex)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
		    if (rec == null) return;

			GEDCOMSex sex = (strSex.Length == 1) ? GKUtils.GetSexBySign(strSex[1]) : GEDCOMSex.svNone;
			rec.Sex = sex;
		}

		public object gt_create_person(string name, string patronymic, string family, string strSex)
		{
			GEDCOMSex sex = (strSex.Length == 1) ? GKUtils.GetSexBySign(strSex[0]) : GEDCOMSex.svNone;

			GEDCOMIndividualRecord iRec = fBase.Context.CreatePersonEx(name, patronymic, family, sex, false);
			return iRec;
		}

		public object gt_create_family()
		{
			GEDCOMFamilyRecord fRec = fBase.Tree.CreateFamily();
			return fRec;
		}

		public object gt_create_note()
		{
			GEDCOMNoteRecord nRec = fBase.Tree.CreateNote();
			return nRec;
		}

		public object gt_create_source(string name)
		{
			GEDCOMSourceRecord srcRec = fBase.Tree.CreateSource();
			srcRec.FiledByEntry = name;
			return srcRec;
		}

		public object gt_create_group(string name)
		{
			GEDCOMGroupRecord grpRec = fBase.Tree.CreateGroup();
			grpRec.GroupName = name;
			return grpRec;
		}

		public void gt_bind_group_member(object groupPtr, object personPtr)
		{
			GEDCOMGroupRecord grp = groupPtr as GEDCOMGroupRecord;
            if (grp == null) return;

			GEDCOMIndividualRecord person = personPtr as GEDCOMIndividualRecord;
			grp.AddMember(person);
		}

		public void gt_add_note_text(object notePtr, string txt)
		{
			GEDCOMNoteRecord nRec = notePtr as GEDCOMNoteRecord;
            if (nRec == null) return;

			nRec.AddNoteText(txt);
		}

		public void gt_bind_record_note(object recPtr, object notePtr)
		{
			GEDCOMRecord rec = recPtr as GEDCOMRecord;
            if (rec == null) return;

			GEDCOMNoteRecord noteRec = notePtr as GEDCOMNoteRecord;
			rec.AddNote(noteRec);
		}

		public void gt_bind_record_source(object recPtr, object srcPtr, string page, int quality)
		{
			GEDCOMRecord rec = recPtr as GEDCOMRecord;
            if (rec == null) return;

			GEDCOMSourceRecord srcRec = srcPtr as GEDCOMSourceRecord;
			rec.AddSource(srcRec, page, quality);
		}

		public void gt_bind_family_spouse(object familyPtr, object spousePtr)
		{
			GEDCOMFamilyRecord fRec = familyPtr as GEDCOMFamilyRecord;
            if (fRec == null) return;
            
            GEDCOMIndividualRecord spRec = spousePtr as GEDCOMIndividualRecord;
			fRec.AddSpouse(spRec);
		}

		public void gt_bind_family_child(object familyPtr, object childPtr)
		{
			GEDCOMFamilyRecord fRec = familyPtr as GEDCOMFamilyRecord;
            if (fRec == null) return;

			GEDCOMIndividualRecord chRec = childPtr as GEDCOMIndividualRecord;
			fRec.AddChild(chRec);
		}

		public string gt_define_sex(string name, string patr)
		{
			GEDCOMSex sx = this.fBase.DefineSex(name, patr);

			return (GKData.SexData[(int)sx].Sign);
		}

		public object gt_find_source(string name)
		{
			GEDCOMSourceRecord srcRec = this.fBase.Context.FindSource(name);
			return srcRec;
		}

		public object gt_create_event(object recPtr, string sign)
		{
			GEDCOMRecordWithEvents rec = recPtr as GEDCOMRecordWithEvents;
			GEDCOMCustomEvent evt = this.fBase.Context.CreateEventEx(rec, sign, "", "");

			return evt;
		}

		public string gt_define_patronymic(string fatherName, string childSex, bool confirm)
		{
			GEDCOMSex sex = (childSex.Length == 1) ? GKUtils.GetSexBySign(childSex[1]) : GEDCOMSex.svNone;

			string childPatronymic = fBase.DefinePatronymic(fatherName, sex, confirm);
			return childPatronymic;
		}

		public object gt_get_person_parents_family(object recPtr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
            if (rec == null) return null;

			GEDCOMFamilyRecord fam = (rec.ChildToFamilyLinks.Count < 1) ? null : rec.ChildToFamilyLinks[0].Family;
			return fam;
		}

		public int gt_get_person_spouses_count(object recPtr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
            return (rec == null) ? 0 : rec.SpouseToFamilyLinks.Count;
		}

		public object gt_get_person_spouse_family(object recPtr, int spIdx)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
            if (rec == null) return null;

            GEDCOMFamilyRecord fam = rec.SpouseToFamilyLinks[spIdx].Family;
			return fam;
		}

		public object gt_get_family_husband(object recPtr)
		{
			GEDCOMFamilyRecord fam = recPtr as GEDCOMFamilyRecord;
			recPtr = (fam == null) ? null : fam.Husband.Value;
			return recPtr;
		}

		public object gt_get_family_wife(object recPtr)
		{
			GEDCOMFamilyRecord fam = recPtr as GEDCOMFamilyRecord;
			recPtr = (fam == null) ? null : fam.Wife.Value;
			return recPtr;
		}

		public int gt_get_family_childs_count(object recPtr)
		{
			GEDCOMFamilyRecord fam = recPtr as GEDCOMFamilyRecord;
			return (fam == null) ? -1 : fam.Childrens.Count;
		}

		public object gt_get_family_child(object recPtr, int childIndex)
		{
			GEDCOMFamilyRecord fam = recPtr as GEDCOMFamilyRecord;
			return (fam == null) ? null : fam.Childrens[childIndex].Value;
		}

		public int gt_get_location_usages(object recPtr)
		{
			GEDCOMLocationRecord loc = recPtr as GEDCOMLocationRecord;
			int usages;

			StringList linkList = new StringList();
			try
			{
				GKUtils.GetLocationLinks(fBase.Tree, loc, ref linkList);
				usages = linkList.Count;
			}
			finally
			{
				linkList.Dispose();
			}

			return usages;
		}

		public int gt_get_record_notes_count(object recPtr)
		{
			GEDCOMRecord rec = recPtr as GEDCOMRecord;
            return (rec == null) ? -1 : rec.Notes.Count;
		}

		//

		public int gt_get_person_groups_count(object recPtr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
			return (rec == null) ? -1 : rec.Groups.Count;
		}

		public object gt_get_person_group(object recPtr, int grIdx)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
            if (rec == null) return null;

			GEDCOMGroupRecord grp = rec.Groups[grIdx].Value as GEDCOMGroupRecord;
			return grp;
		}

		public string gt_get_group_name(object recPtr)
		{
			GEDCOMGroupRecord grp = recPtr as GEDCOMGroupRecord;
			return (grp == null) ? "" : grp.GroupName;
		}

		//

		private DataTable fCSVData = null;

		public bool csv_load(string fileName, bool firstLineIsSchema)
		{
			bool res = false;

			if (File.Exists(fileName))
			{
				try
				{
                    fCSVData = CSVReader.ReadCSVFile(fileName, firstLineIsSchema);
					res = true;
				}
				catch
				{
					res = false;
				}
			}

			return res;
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

	}
}
