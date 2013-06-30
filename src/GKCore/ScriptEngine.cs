using System;
using System.Data;
using System.IO;
using System.Reflection;
using System.Windows.Forms;

using Com.StellmanGreene.CSVReader;
using Ext.Utils;
using GedCom551;
using GKUI;
using LuaInterface;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKCore
{
	public enum TScriptResource { srProgress, srCSV }

	public class ScriptEngine : IDisposable
	{
		private TextBox fDebugOutput;
  		private TfmBase fBase;
		private bool fDisposed;

		public void Dispose()
		{
			if (!this.fDisposed)
			{
				//this.FTree.Dispose();
				this.fDisposed = true;
			}
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
				SysUtils.LogWrite("ScriptEngine.lua_register(" + func_name + "): fail");
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

			///

			lua_register(LVM, "gt_get_person_event_ex");
			lua_register(LVM, "gt_get_event_year");

			///

			lua_register(LVM, "csv_load");
			lua_register(LVM, "csv_close");
			lua_register(LVM, "csv_get_cols");
			lua_register(LVM, "csv_get_rows");
			lua_register(LVM, "csv_get_cell");

			///

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

			///

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

		public void lua_run(string script, TfmBase aBase, TextBox aDebugOutput)
		{
			fDebugOutput = aDebugOutput;
			fBase = aBase;

			using (Lua lvm = new Lua())
			{
				try {
	      			lua_init(lvm);
					lvm.DoString(script);
				} catch (Exception E) {
					lua_print("> "+LangMan.LSList[(int)LSID.LSID_Error]+": " + E.Message);
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
  			TfmProgress.ProgressInit(length, title);
		}

		public void gk_progress_done()
		{
			TfmProgress.ProgressDone();
		}

		public void gk_progress_step()
		{
			TfmProgress.ProgressStep();
		}

		public int gk_strpos(string substr, string str)
		{
			return str.IndexOf(substr);
		}

		public void gk_update_view()
		{
			fBase.ListsRefresh(false);
		}

		public string gk_select_file()
		{
			string fn;

			using (OpenFileDialog dlg = new OpenFileDialog()) {
				if (dlg.ShowDialog() == DialogResult.OK) {
					fn = dlg.FileName;
				} else {
					fn = "";
				}
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

		public int gt_get_record_type(object rec_ptr)
		{
			TGEDCOMRecord rec = rec_ptr as TGEDCOMRecord;
			return (int)rec.RecordType;
		}

		public bool gt_delete_record(object rec_ptr)
		{
			TGEDCOMRecord rec = rec_ptr as TGEDCOMRecord;
			bool res = fBase.DeleteRecord(rec, false);
			return res;
		}

		public string gt_get_record_xref(object rec_ptr)
		{
			TGEDCOMRecord rec = rec_ptr as TGEDCOMRecord;
			return rec.XRef;
		}

		public string gt_get_record_uid(object rec_ptr)
		{
			TGEDCOMRecord rec = rec_ptr as TGEDCOMRecord;
			return rec.UID;
		}

		public string gt_get_record_type_name(int rec_type)
		{
			TGEDCOMRecordType rt = (TGEDCOMRecordType)rec_type;
			string rt_name = rt.ToString();
			return rt_name;
		}

		public bool gt_record_is_filtered(object rec_ptr)
		{
			TGEDCOMRecord rec = rec_ptr as TGEDCOMRecord;
			return fBase.RecordIsFiltered(rec);
		}

		public object gt_select_record(int rec_type)
		{
			TGEDCOMRecord rec = fBase.SelectRecord((TGEDCOMRecordType)rec_type, null);
			return rec;
		}

		public string gt_get_person_name(object rec_ptr)
		{
			TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
			return ((rec == null) ? "" : rec.aux_GetNameStr(true, false));
		}

		public int gt_get_person_associations_count(object rec_ptr)
		{
			TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
			return rec.Associations.Count;
		}

		public object gt_get_person_association(object rec_ptr, int idx)
		{
			TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
			TGEDCOMAssociation asso = rec.Associations[idx];

			return asso;
		}

		public object gt_add_person_association(object rec_ptr, string rel, object a_ptr)
		{
			TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
			TGEDCOMIndividualRecord a_rec = a_ptr as TGEDCOMIndividualRecord;
			TGEDCOMAssociation asso = rec.aux_AddAssociation(rel, a_rec);
			return asso;
		}

		public void gt_delete_person_association(object rec_ptr, int idx)
		{
			TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
			rec.Associations.Delete(idx);
		}

		public int gt_get_person_events_count(object rec_ptr)
		{
			TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
			return rec.IndividualEvents.Count;
		}

		public object gt_get_person_event(object rec_ptr, int idx)
		{
			TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
			TGEDCOMCustomEvent evt = rec.IndividualEvents[idx];
			return evt;
		}

		public object gt_get_person_event_ex(object rec_ptr, string sign)
		{
			TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
			TGEDCOMCustomEvent evt = rec.GetIndividualEvent(sign);
			return evt;
		}

		public void gt_delete_person_event(object rec_ptr, int idx)
		{
			TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
			rec.IndividualEvents.Delete(idx);
		}

		public string gt_get_event_date(object ev_ptr)
		{
			TGEDCOMCustomEvent evt = ev_ptr as TGEDCOMCustomEvent;
			return (TGenEngine.GEDCOMEventToDateStr(evt, TGenEngine.TDateFormat.dfDD_MM_YYYY, false));
		}

		public int gt_get_event_year(object ev_ptr)
		{
			TGEDCOMCustomEvent evt = ev_ptr as TGEDCOMCustomEvent;
			if (evt == null) {
				return 0;
			} else {
				int year;
				ushort month, day;
				evt.Detail.Date.aux_GetIndependentDate(out year, out month, out day);
				return year;
			}
		}

		public void gt_set_event_date(object ev_ptr, string date)
		{
			try
			{
				if (ev_ptr != null && date != "") {
					TGEDCOMCustomEvent evt = ev_ptr as TGEDCOMCustomEvent;
					evt.Detail.Date.ParseString(date);
				}
			}
			catch
			{
				throw new Exception(LangMan.LSList[(int)LSID.LSID_DateFormatInvalid] + ": " + date);
			}
		}

		public string gt_get_event_value(object ev_ptr)
		{
			TGEDCOMCustomEvent evt = ev_ptr as TGEDCOMCustomEvent;
			return evt.StringValue;
		}

		public string gt_get_event_place(object ev_ptr)
		{
			TGEDCOMCustomEvent evt = ev_ptr as TGEDCOMCustomEvent;
			return evt.Detail.Place.StringValue;
		}

		public void gt_set_event_value(object ev_ptr, string value)
		{
			TGEDCOMCustomEvent evt = ev_ptr as TGEDCOMCustomEvent;
			evt.StringValue = value;
		}

		public void gt_set_event_place(object ev_ptr, string place)
		{
			TGEDCOMCustomEvent evt = ev_ptr as TGEDCOMCustomEvent;
			evt.Detail.Place.StringValue = place;
		}

	public string gt_get_event_name(object ev_ptr)
	{
		TGEDCOMCustomEvent evt = ev_ptr as TGEDCOMCustomEvent;
		return evt.Name;
	}

	public string gt_get_person_sex(object rec_ptr)
	{
		TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
		return TGenEngine.SexData[(int)rec.Sex].Sign;
	}

	public void gt_set_person_sex(object rec_ptr, string s_sex)
	{
		TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;

		TGEDCOMSex sex;
		if (s_sex.Length == 1) {
			sex = TGenEngine.GetSexBySign(s_sex[1]);
		} else {
			sex = TGEDCOMSex.svNone;
		}

		rec.Sex = sex;
	}

	public object gt_create_person(string name, string patronymic, string family, string s_sex)
	{
		TGEDCOMSex sex;
		if (s_sex.Length == 1) {
			sex = TGenEngine.GetSexBySign(s_sex[0]);
		} else {
			sex = TGEDCOMSex.svNone;
		}

		TGEDCOMIndividualRecord i_rec = TGenEngine.CreatePersonEx(fBase.Tree, name, patronymic, family, sex, false);
		return i_rec;
	}

	public object gt_create_family()
	{
		TGEDCOMFamilyRecord f_rec = TGenEngine.CreateFamilyEx(fBase.Tree);
		return f_rec;
	}

	public object gt_create_note()
	{
		TGEDCOMNoteRecord n_rec = TGenEngine.CreateNote(fBase.Tree);
		return n_rec;
	}

	public object gt_create_source(string name)
	{
		TGEDCOMSourceRecord src_rec = TGenEngine.CreateSource(fBase.Tree);
		src_rec.FiledByEntry = name;
		return src_rec;
	}

	public object gt_create_group(string name)
	{
		TGEDCOMGroupRecord grp_rec = TGenEngine.CreateGroup(fBase.Tree);
		grp_rec.GroupName = name;
		return grp_rec;
	}

	public void gt_bind_group_member(object group_ptr, object person_ptr)
	{
		TGEDCOMGroupRecord grp = group_ptr as TGEDCOMGroupRecord;
		TGEDCOMIndividualRecord person = person_ptr as TGEDCOMIndividualRecord;
		grp.aux_AddMember(person);
	}

	public void gt_add_note_text(object note_ptr, string txt)
	{
		TGEDCOMNoteRecord n_rec = note_ptr as TGEDCOMNoteRecord;
		TGenEngine.AddNoteText(n_rec, txt);
	}

	public void gt_bind_record_note(object rec_ptr, object note_ptr)
	{
		TGEDCOMRecord rec = rec_ptr as TGEDCOMRecord;
		TGEDCOMNoteRecord note_rec = note_ptr as TGEDCOMNoteRecord;

		rec.aux_AddNote(note_rec);
	}

	public void gt_bind_record_source(object rec_ptr, object src_ptr, string page, int quality)
	{
		TGEDCOMRecord rec = rec_ptr as TGEDCOMRecord;
		TGEDCOMSourceRecord src_rec = src_ptr as TGEDCOMSourceRecord;

		rec.aux_AddSource(src_rec, page, quality);
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
		TGEDCOMSex sx = TfmSexCheck.DefineSex(name, patr, TfmGEDKeeper.Instance.NamesTable);

		return (TGenEngine.SexData[(int)sx].Sign);
	}

	public object gt_find_source(string name)
	{
		TGEDCOMSourceRecord src_rec = fBase.Engine.FindSource(name);
		return src_rec;
	}

	public object gt_create_event(object rec_ptr, string sign)
	{
		TGEDCOMRecord rec = rec_ptr as TGEDCOMIndividualRecord;
		TGEDCOMCustomEvent evt = TGenEngine.CreateEventEx(fBase.Tree, rec, sign, "", "");

		return evt;
	}

	public string gt_define_patronymic(string father_name, string child_sex, bool confirm)
	{
		string child_patronymic;
		TGEDCOMSex sex;

		if (child_sex.Length == 1) {
			sex = TGenEngine.GetSexBySign(child_sex[1]);
		} else {
			sex = TGEDCOMSex.svNone;
		}

		child_patronymic = fBase.DefinePatronymic(father_name, sex, confirm);

		return child_patronymic;
	}

	public object gt_get_person_parents_family(object rec_ptr)
	{
		TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
		TGEDCOMFamilyRecord fam;

		if (rec.ChildToFamilyLinks.Count < 1) {
			fam = null;
		} else {
			fam = rec.ChildToFamilyLinks[0].Family;
		}

		return fam;
	}

	public int gt_get_person_spouses_count(object rec_ptr)
	{
		TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
		return rec.SpouseToFamilyLinks.Count;
	}

	public object gt_get_person_spouse_family(object rec_ptr, int sp_idx)
	{
		TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
		TGEDCOMFamilyRecord fam = rec.SpouseToFamilyLinks[sp_idx].Family;

		return fam;
	}

	public object gt_get_family_husband(object rec_ptr)
	{
		TGEDCOMFamilyRecord fam = rec_ptr as TGEDCOMFamilyRecord;
		if (fam == null) {
			rec_ptr = null;
		} else {
			rec_ptr = fam.Husband.Value;
		}

		return rec_ptr;
	}

	public object gt_get_family_wife(object rec_ptr)
	{
		TGEDCOMFamilyRecord fam = rec_ptr as TGEDCOMFamilyRecord;
		if (fam == null) {
			rec_ptr = null;
		} else {
			rec_ptr = fam.Wife.Value;
		}

		return rec_ptr;
	}

	public int gt_get_family_childs_count(object rec_ptr)
	{
		TGEDCOMFamilyRecord fam = rec_ptr as TGEDCOMFamilyRecord;
		return fam.Childrens.Count;
	}

	public object gt_get_family_child(object rec_ptr, int ch_idx)
	{
		TGEDCOMFamilyRecord fam = rec_ptr as TGEDCOMFamilyRecord;
		return fam.Childrens[ch_idx].Value;
	}

	public int gt_get_location_usages(object rec_ptr)
	{
		TGEDCOMLocationRecord loc = rec_ptr as TGEDCOMLocationRecord;
		int usages = 0;

		StringList link_list = new StringList();
		try
		{
			TGenEngine.GetLocationLinks(fBase.Tree, loc, ref link_list);
			usages = link_list.Count;
		}
		finally
		{
			link_list.Free();
		}

		return usages;
	}

	public int gt_get_record_notes_count(object rec_ptr)
	{
		TGEDCOMRecord rec = rec_ptr as TGEDCOMRecord;
		return rec.Notes.Count;
	}

	//

	public int gt_get_person_groups_count(object rec_ptr)
	{
		TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
		return rec.Groups.Count;
	}

	public object gt_get_person_group(object rec_ptr, int gr_idx)
	{
		TGEDCOMIndividualRecord rec = rec_ptr as TGEDCOMIndividualRecord;
		TGEDCOMGroupRecord grp = rec.Groups[gr_idx].Value as TGEDCOMGroupRecord;
		return grp;
	}

	public string gt_get_group_name(object rec_ptr)
	{
		TGEDCOMGroupRecord grp = rec_ptr as TGEDCOMGroupRecord;
		return grp.GroupName;
	}

	//

	DataTable csv_data = null;

	public bool csv_load(string file_name, bool first_line_is_schema)
	{
		bool res = false;
		if (!File.Exists(file_name)) return res;

		try
		{
			csv_data = CSVReader.ReadCSVFile(file_name, first_line_is_schema);
			res = true;
		}
		catch
		{
			res = false;
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
