using System;
using System.Data;
using System.IO;
using System.Reflection;
using System.Windows.Forms;

using Com.StellmanGreene.CSVReader;
using GKCommon;
using GKCommon.GEDCOM;
using GKCommon.GEDCOM.Enums;
using GKCore.Interfaces;
using GKCore.Types;
using LuaInterface;

namespace GKCore
{
	public enum ScriptResource { srProgress, srCSV }

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

			LVM["rtNone"] = (int)GEDCOMRecordType.rtNone;
			LVM["rtIndividual"] = (int)GEDCOMRecordType.rtIndividual;
			LVM["rtFamily"] = (int)GEDCOMRecordType.rtFamily;
			LVM["rtNote"] = (int)GEDCOMRecordType.rtNote;
			LVM["rtMultimedia"] = (int)GEDCOMRecordType.rtMultimedia;
			LVM["rtSource"] = (int)GEDCOMRecordType.rtSource;
			LVM["rtRepository"] = (int)GEDCOMRecordType.rtRepository;
			LVM["rtGroup"] = (int)GEDCOMRecordType.rtGroup;
			LVM["rtResearch"] = (int)GEDCOMRecordType.rtResearch;
			LVM["rtTask"] = (int)GEDCOMRecordType.rtTask;
			LVM["rtCommunication"] = (int)GEDCOMRecordType.rtCommunication;
			LVM["rtLocation"] = (int)GEDCOMRecordType.rtLocation;
			LVM["rtSubmission"] = (int)GEDCOMRecordType.rtSubmission;
			LVM["rtSubmitter"] = (int)GEDCOMRecordType.rtSubmitter;

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
			return rec.XRef;
		}

		public string gt_get_record_uid(object recPtr)
		{
			GEDCOMRecord rec = recPtr as GEDCOMRecord;
			return rec.UID;
		}

		public string gt_get_record_type_name(int recType)
		{
			GEDCOMRecordType rt = (GEDCOMRecordType)recType;
			string rt_name = rt.ToString();
			return rt_name;
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
			return ((rec == null) ? "" : rec.aux_GetNameStr(true, false));
		}

		public int gt_get_person_associations_count(object recPtr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
			return rec.Associations.Count;
		}

		public object gt_get_person_association(object recPtr, int idx)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
			GEDCOMAssociation asso = rec.Associations[idx];

			return asso;
		}

		public object gt_add_person_association(object recPtr, string rel, object a_ptr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
			GEDCOMIndividualRecord a_rec = a_ptr as GEDCOMIndividualRecord;
			GEDCOMAssociation asso = rec.aux_AddAssociation(rel, a_rec);
			return asso;
		}

		public void gt_delete_person_association(object recPtr, int idx)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
			rec.Associations.Delete(idx);
		}

		public int gt_get_person_events_count(object recPtr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
			return rec.IndividualEvents.Count;
		}

		public object gt_get_person_event(object recPtr, int idx)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
			GEDCOMCustomEvent evt = rec.IndividualEvents[idx];
			return evt;
		}

		public object gt_get_person_event_ex(object recPtr, string sign)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
			GEDCOMCustomEvent evt = rec.GetIndividualEvent(sign);
			return evt;
		}

		public void gt_delete_person_event(object recPtr, int idx)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
			rec.IndividualEvents.Delete(idx);
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
					GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
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
			return evt.StringValue;
		}

		public string gt_get_event_place(object evPtr)
		{
			GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
			return evt.Detail.Place.StringValue;
		}

		public void gt_set_event_value(object evPtr, string value)
		{
			GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
			evt.StringValue = value;
		}

		public void gt_set_event_place(object evPtr, string place)
		{
			GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
			evt.Detail.Place.StringValue = place;
		}

		public string gt_get_event_name(object evPtr)
		{
			GEDCOMCustomEvent evt = evPtr as GEDCOMCustomEvent;
			return evt.Name;
		}

		public string gt_get_person_sex(object recPtr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
			return GKData.SexData[(int)rec.Sex].Sign;
		}

		public void gt_set_person_sex(object recPtr, string s_sex)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;

			GEDCOMSex sex = (s_sex.Length == 1) ? GKUtils.GetSexBySign(s_sex[1]) : GEDCOMSex.svNone;
			rec.Sex = sex;
		}

		public object gt_create_person(string name, string patronymic, string family, string s_sex)
		{
			GEDCOMSex sex = (s_sex.Length == 1) ? GKUtils.GetSexBySign(s_sex[0]) : GEDCOMSex.svNone;

			GEDCOMIndividualRecord i_rec = fBase.Context.CreatePersonEx(name, patronymic, family, sex, false);
			return i_rec;
		}

		public object gt_create_family()
		{
			GEDCOMFamilyRecord fRec = fBase.Tree.aux_CreateFamily();
			return fRec;
		}

		public object gt_create_note()
		{
			GEDCOMNoteRecord nRec = fBase.Tree.aux_CreateNote();
			return nRec;
		}

		public object gt_create_source(string name)
		{
			GEDCOMSourceRecord srcRec = fBase.Tree.aux_CreateSource();
			srcRec.FiledByEntry = name;
			return srcRec;
		}

		public object gt_create_group(string name)
		{
			GEDCOMGroupRecord grpRec = fBase.Tree.aux_CreateGroup();
			grpRec.GroupName = name;
			return grpRec;
		}

		public void gt_bind_group_member(object groupPtr, object personPtr)
		{
			GEDCOMGroupRecord grp = groupPtr as GEDCOMGroupRecord;
			GEDCOMIndividualRecord person = personPtr as GEDCOMIndividualRecord;
			grp.aux_AddMember(person);
		}

		public void gt_add_note_text(object notePtr, string txt)
		{
			GEDCOMNoteRecord nRec = notePtr as GEDCOMNoteRecord;
			nRec.aux_AddNoteText(txt);
		}

		public void gt_bind_record_note(object recPtr, object notePtr)
		{
			GEDCOMRecord rec = recPtr as GEDCOMRecord;
			GEDCOMNoteRecord noteRec = notePtr as GEDCOMNoteRecord;

			rec.AddNote(noteRec);
		}

		public void gt_bind_record_source(object recPtr, object srcPtr, string page, int quality)
		{
			GEDCOMRecord rec = recPtr as GEDCOMRecord;
			GEDCOMSourceRecord srcRec = srcPtr as GEDCOMSourceRecord;

			rec.AddSource(srcRec, page, quality);
		}

		public void gt_bind_family_spouse(object familyPtr, object spousePtr)
		{
			GEDCOMFamilyRecord fRec = familyPtr as GEDCOMFamilyRecord;
			GEDCOMIndividualRecord spRec = spousePtr as GEDCOMIndividualRecord;

			fRec.aux_AddSpouse(spRec);
		}

		public void gt_bind_family_child(object familyPtr, object childPtr)
		{
			GEDCOMFamilyRecord fRec = familyPtr as GEDCOMFamilyRecord;
			GEDCOMIndividualRecord chRec = childPtr as GEDCOMIndividualRecord;

			fRec.aux_AddChild(chRec);
		}

		public string gt_define_sex(string name, string patr)
		{
			GEDCOMSex sx = this.fBase.DefineSex(name, patr);

			return (GKData.SexData[(int)sx].Sign);
		}

		public object gt_find_source(string name)
		{
			GEDCOMSourceRecord srcRec = this.fBase.Context.aux_FindSource(name);
			return srcRec;
		}

		public object gt_create_event(object recPtr, string sign)
		{
			GEDCOMRecord rec = recPtr as GEDCOMIndividualRecord;
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

			GEDCOMFamilyRecord fam = (rec.ChildToFamilyLinks.Count < 1) ? null : rec.ChildToFamilyLinks[0].Family;
			return fam;
		}

		public int gt_get_person_spouses_count(object recPtr)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
			return rec.SpouseToFamilyLinks.Count;
		}

		public object gt_get_person_spouse_family(object recPtr, int spIdx)
		{
			GEDCOMIndividualRecord rec = recPtr as GEDCOMIndividualRecord;
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
			GEDCOMGroupRecord grp = rec.Groups[grIdx].Value as GEDCOMGroupRecord;
			return grp;
		}

		public string gt_get_group_name(object recPtr)
		{
			GEDCOMGroupRecord grp = recPtr as GEDCOMGroupRecord;
			return (grp == null) ? "" : grp.GroupName;
		}

		//

		DataTable csv_data = null;

		public bool csv_load(string fileName, bool firstLineIsSchema)
		{
			bool res = false;

			if (File.Exists(fileName))
			{
				try
				{
					csv_data = CSVReader.ReadCSVFile(fileName, firstLineIsSchema);
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
