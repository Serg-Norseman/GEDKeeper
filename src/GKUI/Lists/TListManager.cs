using System;
using System.Collections.Generic;

using Ext.Utils;
using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public enum TDataType
	{
		dtString,
		dtInteger,
		dtFloat,
		dtDate,
		dtDateTime
	}

	public enum TConditionKind
	{
		ck_NotEq, ck_LT, ck_LET, ck_Eq, ck_GET, ck_GT, ck_Contains, ck_NotContains
	}

	public struct TFilterCondition
	{
		public int col_index;

		public Enum column;
		public TConditionKind condition;
		public object value;
	}

	public class TListFilter
	{
		public List<TFilterCondition> ColumnsFilter = new List<TFilterCondition>();
		
		public TListFilter()
		{
		}
		
		public virtual void Clear()
		{
			ColumnsFilter.Clear();
		}
	}

	public abstract class TListManager : IDisposable
	{
		protected struct TColMapRec
		{
			public byte col_type;
			public byte col_subtype;
		}

		protected TListFilter FFilter;
		protected TGEDCOMTree FTree;
		protected bool Disposed_;

		private TListColumns FListColumns;
		private List<TColMapRec> FColumnsMap;

		public string QuickFilter = "*";

		public TListFilter Filter
		{
			get
			{
				return this.FFilter;
			}
		}

		public TListColumns ListColumns
		{
			get
			{
				return this.FListColumns;
			}
		}

		public TListManager(TGEDCOMTree aTree)
		{
			this.FTree = aTree;
			this.FListColumns = GetDefaultListColumns();
			this.FColumnsMap = new List<TColMapRec>();

			CreateFilter();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Disposed_ = true;
			}
		}

		protected void AddListColumn(GKListView aList, string caption, int width, bool autoSize, byte colType, byte colSubType)
		{
			aList.AddListColumn(caption, width, autoSize);

			TColMapRec cr = new TColMapRec();
			cr.col_type = colType;
			cr.col_subtype = colSubType;
			FColumnsMap.Add(cr);
		}

		protected void ColumnsMap_Clear()
		{
			FColumnsMap.Clear();
		}

		protected virtual void CreateFilter()
		{
			this.FFilter = new TListFilter();
		}

		protected bool IsMatchesMask(string S, string Mask)
		{
			bool result = false;
			if (S != null && Mask != null && S != "" && Mask != "")
			{
				string stx = S.ToLower();
				string[] masks = Mask.ToLower().Split(new char[] { '|' });

				int num = masks.Length - 1;
				for (int i = 0; i <= num; i++)
				{
					result = (result || TGenEngine.MatchesMask(stx, masks[i]));
				}
			}
			return result;
		}

		public void UpdateTitles(GKListView listView, bool isMain)
		{
            if (listView == null) return;

			listView.BeginUpdate();
			try
			{
				listView.Columns.Clear();
				this.UpdateColumns(listView, isMain);
			}
			finally
			{
				listView.EndUpdate();
			}
		}

		public abstract bool CheckFilter(TShieldState aShieldState);
		public abstract void Fetch(TGEDCOMRecord aRec);

		public object GetColumnValue(int col_index)
		{
			// col_index - from 1
			TColMapRec colrec = this.FColumnsMap[col_index];
			return GetColumnValueEx(colrec.col_type, colrec.col_subtype);
		}

		protected virtual object GetColumnValueEx(int col_type, int col_subtype)
		{
			return null;
		}

		public virtual void InitFilter()
		{
		}

		public virtual void UpdateItem(GKListItem item, bool isMain)
		{
			if (item == null) return;

			for (int i = 1; i < FColumnsMap.Count; i++)
			{
				TColMapRec colrec = this.FColumnsMap[i];

				// aColIndex - from 1
				TColumnStatic cs = this.FListColumns.ColumnStatics[colrec.col_type];
				object val = GetColumnValueEx(colrec.col_type, colrec.col_subtype);
				string res = ConvColValue(val, cs);

				item.SubItems.Add(res);
			}
		}

		protected virtual void UpdateColumns(GKListView listView, bool isMain)
		{
            if (listView == null) return;

			this.ColumnsMap_Clear();
			this.AddListColumn(listView, "№", 50, false, 0, 0);

			for (int i = 0; i < this.FListColumns.ColumnStatics.Count; i++) {
				TColumnStatic cs = this.FListColumns.ColumnStatics[i];
				this.AddListColumn(listView, cs.colName, cs.width, false, (byte)i, 0);
			}
		}

		//

		public string GetColumnName(Enum colType)
		{
			int col = (colType as IConvertible).ToByte(null);
			if (col >= 0 && col < FListColumns.ColumnStatics.Count) {
				return FListColumns.ColumnStatics[col].colName;
			} else {
				return "<?>";
			}
		}

		public TDataType GetColumnDataType(int index)
		{
			int col = index - 1;
			if (col >= 0 && col < FListColumns.ColumnStatics.Count) {
				return FListColumns.ColumnStatics[col].dataType;
			} else {
				return TDataType.dtString;
			}
		}

		public abstract TListColumns GetDefaultListColumns();

		/// <summary>
		/// Используется в блоке настройки общей фильтрации, TfmComFilter
		/// </summary>
		public abstract Type GetColumnsEnum();

		private static string ConvColValue(object val, TColumnStatic cs)
		{
			switch (cs.dataType) {
				case TDataType.dtString:
					return val.ToString();

				case TDataType.dtInteger:
					return val.ToString();

				case TDataType.dtDate:
					return val.ToString();

				case TDataType.dtDateTime:
					DateTime dtx = ((DateTime)val);
					return ((dtx.Ticks == 0) ? "" : dtx.ToString("yyyy.MM.dd HH:mm:ss", null));

				case TDataType.dtFloat:
					return ((double)val).ToString(cs.format, cs.nfi);
			}

			return val.ToString();
		}

		private static object ConvColStr(string val, TDataType type)
		{
			switch (type) {
				case TDataType.dtString:
					return val;
				case TDataType.dtInteger:
					return SysUtils.ParseInt(val, 0);
				case TDataType.dtFloat:
					return SysUtils.ParseFloat(val, 0.0);
				case TDataType.dtDate:
				case TDataType.dtDateTime:
					return DateTime.Parse(val);
			}
			return val;
		}

		public void AddCondition(Enum column, TConditionKind condition, string value)
		{
			int col = (column as IConvertible).ToByte(null);

			TFilterCondition fltCond = new TFilterCondition();
			fltCond.column = column;
			fltCond.col_index = col;
			fltCond.condition = condition;
			fltCond.value = ConvColStr(value, this.GetColumnDataType(col));
			this.Filter.ColumnsFilter.Add(fltCond);
		}

		private bool CheckCondition(TFilterCondition fcond)
		{
			bool res = true;

			object dataval = this.GetColumnValueEx(fcond.col_index, -1);

			int comp_res = 0;
			if (fcond.condition != TConditionKind.ck_Contains) {
				comp_res = (dataval as IComparable).CompareTo(fcond.value);
			}

			switch (fcond.condition) {
				case TConditionKind.ck_NotEq:
					res = comp_res != 0;
					break;
				case TConditionKind.ck_LT:
					res = comp_res < 0;
					break;
				case TConditionKind.ck_LET:
					res = comp_res <= 0;
					break;
				case TConditionKind.ck_Eq:
					res = comp_res == 0;
					break;
				case TConditionKind.ck_GET:
					res = comp_res >= 0;
					break;
				case TConditionKind.ck_GT:
					res = comp_res > 0;
					break;
				case TConditionKind.ck_Contains:
					res = TGenEngine.MatchesMask(dataval.ToString(), "*" + fcond.value.ToString() + "*");
					break;
				case TConditionKind.ck_NotContains:
					res = !TGenEngine.MatchesMask(dataval.ToString(), "*" + fcond.value.ToString() + "*");
					break;
			}
			return res;
		}

		protected bool CheckNewFilter()
		{
			bool res = true;

			if (this.Filter.ColumnsFilter.Count > 0) {
				for (int i = 0; i < this.Filter.ColumnsFilter.Count; i++) {
					TFilterCondition fcond = this.Filter.ColumnsFilter[i];
					res = res && this.CheckCondition(fcond);
				}
			}

			return res;
		}
	}
}
