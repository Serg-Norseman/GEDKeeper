using System;
using System.Collections.Generic;
using System.Globalization;

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

	public struct TColumnStatic
	{
		//public byte colType;
		public string colName;
		public TDataType dataType;
		public NumberFormatInfo nfi;
		public string format;
		public int width;

		public TColumnStatic(/*Enum colType*/ string colName, TDataType dataType, int width)
		{
			//this.colType = ((IConvertible)colType).ToByte(null);
			this.colName = colName;
			this.dataType = dataType;
			this.width = width;
			this.nfi = null;
			this.format = null;
		}

		public TColumnStatic(/*Enum colType*/ string colName, TDataType dataType, int width, string format, NumberFormatInfo nfi)
		{
			//this.colType = ((IConvertible)colType).ToByte(null);
			this.colName = colName;
			this.dataType = dataType;
			this.width = width;
			this.nfi = nfi;
			this.format = format;
		}
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
		protected TListFilter FFilter;
		protected TGEDCOMTree FTree;
		protected bool Disposed_;

		protected List<TColumnStatic> ColumnStatics = new List<TColumnStatic>();

		public string QuickFilter = "*";

		public TListFilter Filter
		{
			get
			{
				return this.FFilter;
			}
		}

		public TListManager(TGEDCOMTree aTree)
		{
			this.FTree = aTree;

			InitColumnStatics();
			CreateFilter();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Disposed_ = true;
			}
		}

		protected virtual void InitColumnStatics()
		{
			// dummy
		}

		protected virtual void CreateFilter()
		{
			this.FFilter = new TListFilter();
		}

		public bool IsMatchesMask(string S, string Mask)
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

		public abstract bool CheckFilter(TGenEngine.TShieldState aShieldState);
		public abstract void Fetch(TGEDCOMRecord aRec);

		protected string GetColumnValue(int col_index, bool isMain)
		{
			// aColIndex - from 1
			TColumnStatic cs = this.ColumnStatics[col_index - 1];

			object val = GetColumnValueEx(col_index);

			string res = ConvColValue(val, cs);

			return res;
		}

		public virtual object GetColumnValueEx(int col_index)
		{
			// col_index - from 1
			return GetColumnValueDirect(col_index - 1, 0);
		}

		public virtual object GetColumnValueDirect(int col_type, int col_subtype)
		{
			return null;
		}

		public virtual void InitFilter()
		{
		}

		public virtual void UpdateItem(GKListItem item, bool isMain)
		{
            if (item == null) return;

			for (int i = 1; i <= this.ColumnStatics.Count; i++) {
				item.SubItems.Add(this.GetColumnValue(i, isMain));
			}
		}

		public virtual void UpdateColumns(GKListView listView, bool isMain)
		{
            if (listView == null) return;

			listView.AddListColumn("¹", 50, false);

			for (int i = 0; i < this.ColumnStatics.Count; i++) {
				TColumnStatic cs = this.ColumnStatics[i];
				listView.AddListColumn(cs.colName, cs.width, false);
			}
		}

		//

		public string GetColumnName(Enum colType)
		{
			int col = ((IConvertible)colType).ToByte(null);
			if (col >= 0 && col < ColumnStatics.Count) {
				return ColumnStatics[col].colName;
			} else {
				return "<?>";
			}
		}

		public TDataType GetColumnDataType(int index)
		{
			int col = index - 1;
			if (col >= 0 && col < ColumnStatics.Count) {
				return ColumnStatics[col].dataType;
			} else {
				return TDataType.dtString;
			}
		}

		public abstract TListColumns GetDefaultListColumns();

		public abstract Type GetColumnsEnum();

		protected static string ConvColValue(object val, TColumnStatic cs)
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

		protected static object ConvColStr(string val, TDataType type)
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
			int col = ((IConvertible)column).ToByte(null);

			TFilterCondition flt_col = new TFilterCondition();
			flt_col.column = column;
			flt_col.col_index = col;
			flt_col.condition = condition;
			flt_col.value = ConvColStr(value, this.GetColumnDataType(col));
			this.Filter.ColumnsFilter.Add(flt_col);
		}

		private bool CheckCondition(TFilterCondition fcond)
		{
			bool res = true;

			object dataval = this.GetColumnValueDirect(fcond.col_index, -1);

			int comp_res = 0;
			if (fcond.condition != TConditionKind.ck_Contains) {
				comp_res = ((IComparable)dataval).CompareTo(fcond.value);
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
