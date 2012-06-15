using System;

using GedCom551;
using GKCore;
using GKUI.Controls;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public enum TNoteColumnType : byte
	{
		nctText,
		nctChangeDate
	}

	public sealed class TNoteListMan : TListManager
	{
		private TGEDCOMNoteRecord FRec;

		public override bool CheckFilter(TPersonsFilter aFilter, TGenEngine.TShieldState aShieldState)
		{
			bool res = base.CheckNewFilter();
			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMNoteRecord);
		}

		public override object GetColumnValueEx(int col_index)
		{
			switch (col_index) {
				case 1:
					{
						string st;
						if (this.FRec.Note.Count > 0)
						{
							st = this.FRec.Note[0].Trim();
							if (st == "" && this.FRec.Note.Count > 1)
							{
								st = this.FRec.Note[1].Trim();
							}
						}
						else
						{
							st = "";
						}
						return st;
					}
				case 2:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		protected override void InitColumnStatics()
		{
			this.ColumnStatics.Clear();
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[108], TDataType.dtString, 400));
			this.ColumnStatics.Add(new TColumnStatic(LangMan.LSList[317], TDataType.dtDateTime, 150));
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TNoteColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return null;
		}

		public TNoteListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
