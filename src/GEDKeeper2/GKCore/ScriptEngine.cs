using System;
using System.Data;
using System.IO;
using System.Reflection;
using System.Windows.Forms;

using Com.StellmanGreene.CSVReader;
using ExtUtils;
using GedCom551;
using GKCore.Interfaces;
using LuaInterface;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public enum TScriptResource { srProgress, srCSV }

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

	public class ScriptEngine : BaseObject
	{
		private TextBox fDebugOutput;
		private IBase fBase;

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
			fDebugOutput.Text += (s + "\r\n");
		}

		private void lua_register(Lua lvm, string func_name)
		{
			MethodInfo mInfo = this.GetType().GetMethod(func_name);
			if (mInfo != null) {
				lvm.RegisterFunction(func_name, this, mInfo);
			}
			else
			{
				this.fBase.Host.LogWrite("ScriptEngine.lua_register(" + func_name + "): fail");
			}
		}

		private void lua_init(Lua LVM)
		{
			lua_register(LVM, "gk_print");
			lua_register(LVM, "gk_progress_init");
			lua_register(LVM, "gk_progress_done");
			lua_register(LVM, "gk_progress_step");

			lua_register(LVM, "gk_strpos");
			lua_register(LVM, "gk_update_view");
			lua_register(LVM, "gk_select_file");

			LVM["rtNone"] = (int)TGEDCOMRecordType.rtNone;
			LVM["rtIndividual"] = (int)TGEDCOMRecordType.rtIndividual;
			LVM["rtFamily"] = (int)TGEDCOMRecordType.rtFamily;
			LVM["rtNote"] = (int)TGEDCOMRecordType.rtNote;
			LVM["rtMultimedia"] = (int)TGEDCOMRecordType.rtMultimedia;
			LVM["rtSource"] = (int)TGEDCOMRecordType.rtSource;
			LVM["rtRepository"] = (int)TGEDCOMRecordType.rtRepository;
			LVM["rtGroup"] = (int)TGEDCOMRecordType.rtGroup;
			LVM["rtResearch"] = (int)TGEDCOMRecordType.rtResearch;
			LVM["rtTask"] = (int)TGEDCOMRecordType.rtTask;
			LVM["rtCommunication"] = (int)TGEDCOMRecordType.rtCommunication;
			LVM["rtLocation"] = (int)TGEDCOMRecordType.rtLocation;
			LVM["rtSubmission"] = (int)TGEDCOMRecordType.rtSubmission;
			LVM["rtSubmitter"] = (int)TGEDCOMRecordType.rtSubmitter;

			lua_register(LVM, "gt_get_records_count");
			lua_register(LVM, "gt_get_record");
			lua_register(LVM, "gt_get_record_type");
			lua_register(LVM, "gt_get_record_type_name");
			lua_register(LVM, "gt_get_record_xref");
			lua_register(LVM, "gt_get_record_uid");

			lua_register(LVM, "gt_delete_record");
			lua_register(LVM, "gt_record_is_filtered");
			lua_register(LVM, "gt_select_record");

			lua_register(LVM, "gt_create_person");
			lua_register(LVM, "gt_create_family");
			lua_register(LVM, "gt_create_note");

			lua_register(LVM, "gt_get_person_name");
			lua_register(LVM, "gt_define_sex");

			lua_register(LVM, "gt_get_person_associations_count");
			lua_register(LVM, "gt_get_person_association");
			lua_register(LVM, "gt_delete_person_association");

			lua_register(LVM, "gt_get_person_events_count");
			lua_register(LVM, "gt_get_person_event");
			lua_register(LVM, "gt_delete_person_event");

			lua_register(LVM, "gt_bind_record_note");
			lua_register(LVM, "gt_bind_record_source");

			lua_register(LVM, "gt_bind_family_spouse");
			lua_register(LVM, "gt_bind_family_child");

			lua_register(LVM, "gt_add_note_text");

			lua_register(LVM, "gt_create_event");

			lua_register(LVM, "gt_get_event_value");
			lua_register(LVM, "gt_get_event_place");
			lua_register(LVM, "gt_get_event_date");
			lua_register(LVM, "gt_get_event_name");

			lua_register(LVM, "gt_set_event_value");
			lua_register(LVM, "gt_set_event_place");
			lua_register(LVM, "gt_set_event_date");

			lua_register(LVM, "gt_create_source");
			lua_register(LVM, "gt_find_source");

			lua_register(LVM, "gt_create_group");
			lua_register(LVM, "gt_bind_group_member");

			//

			lua_register(LVM, "gt_get_person_event_ex");
			lua_register(LVM, "gt_get_event_year");

			//

			lua_register(LVM, "csv_load");
			lua_register(LVM, "csv_close");
			lua_register(LVM, "csv_get_cols");
			lua_register(LVM, "csv_get_rows");
			lua_register(LVM, "csv_get_cell");

			//

			lua_register(LVM, "gt_add_person_association");
			lua_register(LVM, "gt_define_patronymic");
			lua_register(LVM, "gt_get_person_parents_family");
			lua_register(LVM, "gt_get_person_spouses_count");
			lua_register(LVM, "gt_get_person_spouse_family");
			lua_register(LVM, "gt_get_family_husband");
			lua_register(LVM, "gt_get_family_wife");
			lua_register(LVM, "gt_get_family_childs_count");
			lua_register(LVM, "gt_get_family_child");

			lua_register(LVM, "gt_get_location_usages");
			lua_register(LVM, "gt_get_record_notes_count");
			lua_register(LVM, "gt_get_person_sex");
			lua_register(LVM, "gt_set_person_sex");

			lua_register(LVM, "gt_get_person_groups_count");
			lua_register(LVM, "gt_get_person_group");
			lua_register(LVM, "gt_get_group_name");

			// experimentals

			lua_register(LVM, "ado_open");
			lua_register(LVM, "ado_close");
			lua_register(LVM, "ado_query_open");
			lua_register(LVM, "ado_query_close");
			lua_register(LVM, "ado_query_first");
			lua_register(LVM, "ado_query_prev");
			lua_register(LVM, "ado_query_next");
			lua_register(LVM, "ado_query_last");
			lua_register(LVM, "ado_get_query_field");
			lua_register(LVM, "ado_dump");
		}

		public void lua_run(string script, IBase aBase, TextBox aDebugOutput)
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
			TGEDCOMRecord rec = recPtr as TGEDCOMRecord;
			return (rec == null) ? (int)TGEDCOMRecordType.rtNone : (int)rec.RecordType;
		}

		public bool gt_delete_record(object recPtr)
		{
			TGEDCOMRecord rec = recPtr as TGEDCOMRecord;
			bool res = fBase.RecordDelete(rec, false);
			return res;
		}

		public string gt_get_record_xref(object recPtr)
		{
			TGEDCOMRecord rec = recPtr as TGEDCOMRecord;
			return rec.XRef;
		}

		public string gt_get_record_uid(object recPtr)
		{
			TGEDCOMRecord rec = recPtr as TGEDCOMRecord;
			return rec.UID;
		}

		public string gt_get_record_type_name(int recType)
		{
			TGEDCOMRecordType rt = (TGEDCOMRecordType)recType;
			string rt_name = rt.ToString();
			return rt_name;
		}

		public bool gt_record_is_filtered(object recPtr)
		{
			TGEDCOMRecord rec = recPtr as TGEDCOMRecord;
			return fBase.RecordIsFiltered(rec);
		}

		public object gt_select_record(int recType)
		{
			TGEDCOMRecord rec = fBase.SelectRecord((TGEDCOMRecordType)recType, null);
			return rec;
		}

		public string gt_get_person_name(object recPtr)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			return ((rec == null) ? "" : rec.aux_GetNameStr(true, false));
		}

		public int gt_get_person_associations_count(object recPtr)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			return rec.Associations.Count;
		}

		public object gt_get_person_association(object recPtr, int idx)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			TGEDCOMAssociation asso = rec.Associations[idx];

			return asso;
		}

		public object gt_add_person_association(object recPtr, string rel, object a_ptr)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			TGEDCOMIndividualRecord a_rec = a_ptr as TGEDCOMIndividualRecord;
			TGEDCOMAssociation asso = rec.aux_AddAssociation(rel, a_rec);
			return asso;
		}

		public void gt_delete_person_association(object recPtr, int idx)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			rec.Associations.Delete(idx);
		}

		public int gt_get_person_events_count(object recPtr)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			return rec.IndividualEvents.Count;
		}

		public object gt_get_person_event(object recPtr, int idx)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			TGEDCOMCustomEvent evt = rec.IndividualEvents[idx];
			return evt;
		}

		public object gt_get_person_event_ex(object recPtr, string sign)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			TGEDCOMCustomEvent evt = rec.GetIndividualEvent(sign);
			return evt;
		}

		public void gt_delete_person_event(object recPtr, int idx)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			rec.IndividualEvents.Delete(idx);
		}

		public string gt_get_event_date(object evPtr)
		{
			TGEDCOMCustomEvent evt = evPtr as TGEDCOMCustomEvent;
			return (GKUtils.GEDCOMEventToDateStr(evt, DateFormat.dfDD_MM_YYYY, false));
		}

		public int gt_get_event_year(object evPtr)
		{
			TGEDCOMCustomEvent evt = evPtr as TGEDCOMCustomEvent;
			if (evt == null) {
				return 0;
			}

			int year;
			ushort month, day;
			evt.Detail.Date.aux_GetIndependentDate(out year, out month, out day);
			return year;
		}

		public void gt_set_event_date(object evPtr, string date)
		{
			try
			{
				if (evPtr != null && date != "") {
					TGEDCOMCustomEvent evt = evPtr as TGEDCOMCustomEvent;
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
			TGEDCOMCustomEvent evt = evPtr as TGEDCOMCustomEvent;
			return evt.StringValue;
		}

		public string gt_get_event_place(object evPtr)
		{
			TGEDCOMCustomEvent evt = evPtr as TGEDCOMCustomEvent;
			return evt.Detail.Place.StringValue;
		}

		public void gt_set_event_value(object evPtr, string value)
		{
			TGEDCOMCustomEvent evt = evPtr as TGEDCOMCustomEvent;
			evt.StringValue = value;
		}

		public void gt_set_event_place(object evPtr, string place)
		{
			TGEDCOMCustomEvent evt = evPtr as TGEDCOMCustomEvent;
			evt.Detail.Place.StringValue = place;
		}

		public string gt_get_event_name(object evPtr)
		{
			TGEDCOMCustomEvent evt = evPtr as TGEDCOMCustomEvent;
			return evt.Name;
		}

		public string gt_get_person_sex(object recPtr)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			return GKData.SexData[(int)rec.Sex].Sign;
		}

		public void gt_set_person_sex(object recPtr, string s_sex)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;

			TGEDCOMSex sex = (s_sex.Length == 1) ? GKUtils.GetSexBySign(s_sex[1]) : TGEDCOMSex.svNone;
			rec.Sex = sex;
		}

		public object gt_create_person(string name, string patronymic, string family, string s_sex)
		{
			TGEDCOMSex sex = (s_sex.Length == 1) ? GKUtils.GetSexBySign(s_sex[0]) : TGEDCOMSex.svNone;

			TGEDCOMIndividualRecord i_rec = fBase.Context.CreatePersonEx(name, patronymic, family, sex, false);
			return i_rec;
		}

		public object gt_create_family()
		{
			TGEDCOMFamilyRecord fRec = fBase.Tree.aux_CreateFamily();
			return fRec;
		}

		public object gt_create_note()
		{
			TGEDCOMNoteRecord nRec = fBase.Tree.aux_CreateNote();
			return nRec;
		}

		public object gt_create_source(string name)
		{
			TGEDCOMSourceRecord srcRec = fBase.Tree.aux_CreateSource();
			srcRec.FiledByEntry = name;
			return srcRec;
		}

		public object gt_create_group(string name)
		{
			TGEDCOMGroupRecord grpRec = fBase.Tree.aux_CreateGroup();
			grpRec.GroupName = name;
			return grpRec;
		}

		public void gt_bind_group_member(object groupPtr, object personPtr)
		{
			TGEDCOMGroupRecord grp = groupPtr as TGEDCOMGroupRecord;
			TGEDCOMIndividualRecord person = personPtr as TGEDCOMIndividualRecord;
			grp.aux_AddMember(person);
		}

		public void gt_add_note_text(object notePtr, string txt)
		{
			TGEDCOMNoteRecord nRec = notePtr as TGEDCOMNoteRecord;
			nRec.aux_AddNoteText(txt);
		}

		public void gt_bind_record_note(object recPtr, object notePtr)
		{
			TGEDCOMRecord rec = recPtr as TGEDCOMRecord;
			TGEDCOMNoteRecord noteRec = notePtr as TGEDCOMNoteRecord;

			rec.aux_AddNote(noteRec);
		}

		public void gt_bind_record_source(object recPtr, object srcPtr, string page, int quality)
		{
			TGEDCOMRecord rec = recPtr as TGEDCOMRecord;
			TGEDCOMSourceRecord srcRec = srcPtr as TGEDCOMSourceRecord;

			rec.aux_AddSource(srcRec, page, quality);
		}

		public void gt_bind_family_spouse(object f_ptr, object sp_ptr)
		{
			TGEDCOMFamilyRecord f_rec = f_ptr as TGEDCOMFamilyRecord;
			TGEDCOMIndividualRecord sp_rec = sp_ptr as TGEDCOMIndividualRecord;

			f_rec.aux_AddSpouse(sp_rec);
		}

		public void gt_bind_family_child(object f_ptr, object ch_ptr)
		{
			TGEDCOMFamilyRecord f_rec = f_ptr as TGEDCOMFamilyRecord;
			TGEDCOMIndividualRecord ch_rec = ch_ptr as TGEDCOMIndividualRecord;

			f_rec.aux_AddChild(ch_rec);
		}

		public string gt_define_sex(string name, string patr)
		{
			TGEDCOMSex sx = this.fBase.DefineSex(name, patr);

			return (GKData.SexData[(int)sx].Sign);
		}

		public object gt_find_source(string name)
		{
			TGEDCOMSourceRecord srcRec = this.fBase.Context.aux_FindSource(name);
			return srcRec;
		}

		public object gt_create_event(object recPtr, string sign)
		{
			TGEDCOMRecord rec = recPtr as TGEDCOMIndividualRecord;
			TGEDCOMCustomEvent evt = this.fBase.Context.CreateEventEx(rec, sign, "", "");

			return evt;
		}

		public string gt_define_patronymic(string fatherName, string childSex, bool confirm)
		{
			TGEDCOMSex sex = (childSex.Length == 1) ? GKUtils.GetSexBySign(childSex[1]) : TGEDCOMSex.svNone;

			string childPatronymic = fBase.DefinePatronymic(fatherName, sex, confirm);
			return childPatronymic;
		}

		public object gt_get_person_parents_family(object recPtr)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;

			TGEDCOMFamilyRecord fam = (rec.ChildToFamilyLinks.Count < 1) ? null : rec.ChildToFamilyLinks[0].Family;
			return fam;
		}

		public int gt_get_person_spouses_count(object recPtr)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			return rec.SpouseToFamilyLinks.Count;
		}

		public object gt_get_person_spouse_family(object recPtr, int sp_idx)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			TGEDCOMFamilyRecord fam = rec.SpouseToFamilyLinks[sp_idx].Family;

			return fam;
		}

		public object gt_get_family_husband(object recPtr)
		{
			TGEDCOMFamilyRecord fam = recPtr as TGEDCOMFamilyRecord;
			recPtr = (fam == null) ? null : fam.Husband.Value;
			return recPtr;
		}

		public object gt_get_family_wife(object recPtr)
		{
			TGEDCOMFamilyRecord fam = recPtr as TGEDCOMFamilyRecord;
			recPtr = (fam == null) ? null : fam.Wife.Value;
			return recPtr;
		}

		public int gt_get_family_childs_count(object recPtr)
		{
			TGEDCOMFamilyRecord fam = recPtr as TGEDCOMFamilyRecord;
			return (fam == null) ? -1 : fam.Childrens.Count;
		}

		public object gt_get_family_child(object recPtr, int childIndex)
		{
			TGEDCOMFamilyRecord fam = recPtr as TGEDCOMFamilyRecord;
			return (fam == null) ? null : fam.Childrens[childIndex].Value;
		}

		public int gt_get_location_usages(object recPtr)
		{
			TGEDCOMLocationRecord loc = recPtr as TGEDCOMLocationRecord;
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
			TGEDCOMRecord rec = recPtr as TGEDCOMRecord;
            return (rec == null) ? -1 : rec.Notes.Count;
		}

		//

		public int gt_get_person_groups_count(object recPtr)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			return (rec == null) ? -1 : rec.Groups.Count;
		}

		public object gt_get_person_group(object recPtr, int gr_idx)
		{
			TGEDCOMIndividualRecord rec = recPtr as TGEDCOMIndividualRecord;
			TGEDCOMGroupRecord grp = rec.Groups[gr_idx].Value as TGEDCOMGroupRecord;
			return grp;
		}

		public string gt_get_group_name(object recPtr)
		{
			TGEDCOMGroupRecord grp = recPtr as TGEDCOMGroupRecord;
			return (grp == null) ? "" : grp.GroupName;
		}

		//

		DataTable csv_data = null;

		public bool csv_load(string fileName, bool first_line_is_schema)
		{
			bool res = false;

			if (File.Exists(fileName))
			{
				try
				{
					csv_data = CSVReader.ReadCSVFile(fileName, first_line_is_schema);
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
				csv_data.Dispose();
			}
			catch
			{
			}
		}

		public int csv_get_cols()
		{
			return csv_data.Columns.Count;
		}

		public int csv_get_rows()
		{
			return csv_data.Rows.Count;
		}

		public string csv_get_cell(int col, int row)
		{
			DataRow dr = csv_data.Rows[row];
			return dr.ItemArray[col].ToString();
		}

		public object ado_open(string constr)
		{
			/*try
  {
    TADOConnection con;
    con := TADOConnection.Create(nil);
    con.ConnectionString := constr;
    con.Mode := cmRead;
    con.LoginPrompt := False;
    con.Connected := True;

    lua_pushptr(LVM, con);
  }
  catch
  {
    on E: Exception do lua_DoError(LVM, "ADOFail: " + E.Message);
  }*/
			return null;
		}

		public bool ado_close(object conptr)
		{
			/*try
  {
    TADOConnection(conptr).Free;
    lua_pushboolean(LVM, True);
  }
  catch
  {
    on E: Exception do lua_DoError(LVM, "ADOFail: " + E.Message);
  }*/
			return false;
		}

		public object ado_query_open(object conptr, string query)
		{
			/*try
  {
    q_obj: TADOQuery;
    q_obj := TADOQuery.Create(nil);
    q_obj.Connection := TADOConnection(conptr);
    q_obj.SQL.Text := query;
    q_obj.Open;

    lua_pushptr(LVM, q_obj);
  }
  catch
  {
    on E: Exception do lua_DoError(LVM, "ADOFail: " + E.Message);
  }*/
			return null;
		}

		public bool ado_query_close(object qptr)
		{
			/*try
  {
    TADOQuery(qptr).Free;
    lua_pushboolean(LVM, True);
  }
  catch
  {
    on E: Exception do lua_DoError(LVM, "ADOFail: " + E.Message);
  }*/
			return false;
		}

		public bool ado_query_first(object qptr)
		{
			/*try
  {
    TADOQuery(qptr).First;
    lua_pushboolean(LVM, True);
  }
  catch
  {
    on E: Exception do lua_DoError(LVM, "ADOFail: " + E.Message);
  }*/
			return false;
		}

		public bool ado_query_prev(object qptr)
		{
			/*try
  {
    TADOQuery(qptr).Prior;
    lua_pushboolean(LVM, True);
  }
  catch
  {
    on E: Exception do lua_DoError(LVM, "ADOFail: " + E.Message);
  }*/
			return false;
		}

		public bool ado_query_next(object qptr)
		{
			/*try
  {
    TADOQuery(qptr).Next;
    lua_pushboolean(LVM, True);
  }
  catch
  {
    on E: Exception do lua_DoError(LVM, "ADOFail: " + E.Message);
  }*/
			return false;
		}

		public bool ado_query_last(object qptr)
		{
			/*try
  {
    TADOQuery(qptr).Last;
    lua_pushboolean(LVM, True);
  }
  catch
  {
    on E: Exception do lua_DoError(LVM, "ADOFail: " + E.Message);
  }*/
			return false;
		}

		public string ado_get_query_field(object qptr, string fname)
		{
			/*try
  {
    string fval = TADOQuery(qptr).FieldByName(fname).AsString;
    lua_pushstring(LVM, PChar(fval));
  }
  catch
  {
    on E: Exception do lua_DoError(LVM, "ADOFail: " + E.Message);
  }*/
			return "";
		}

		public void ado_dump(object conptr)
		{
			//string query;
			//StringList tables, fields;

			try
			{
				/*TADOConnection con = TADOConnection(conptr);

		    tables = StringList.Create;
		    fields = StringList.Create;
    		try
      			con.GetTableNames(tables, False);

      			lua_print("Tables:");
      			for (i = 0; i <= tables.Count - 1; i++) {
        			lua_print("  [ " + tables[i] + " ]");

        			con.GetFieldNames(tables[i], fields);
        			for (k = 0; k <= fields.Count - 1; k++) {
          				lua_print("    - " + fields[k]);
        			}
      			}
    		finally
      			fields.Free;
      			tables.Free;
    		}*/
			}
			catch (Exception /*E*/)
			{
				//lua_DoError(LVM, "ADOFail: " + E.Message);
			}
		}

	}
}
