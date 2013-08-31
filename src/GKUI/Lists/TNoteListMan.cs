using System;

using GedCom551;
using GKCore;

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

	public sealed class TNoteListColumns : TListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LangMan.LSList[108], TDataType.dtString, 400, true);
			this.AddStatic(LangMan.LSList[317], TDataType.dtDateTime, 150, true);
		}

		public TNoteListColumns() : base()
		{
			InitData(typeof(TNoteColumnType));
		}
	}

	public sealed class TNoteListMan : TListManager
	{
		private TGEDCOMNoteRecord FRec;

		public override bool CheckFilter(TShieldState aShieldState)
		{
			bool res = base.CheckNewFilter();
			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMNoteRecord);
		}

		protected override object GetColumnValueEx(int col_type, int col_subtype)
		{
			switch (col_type) {
				case 0:
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
				case 1:
					return this.FRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(TNoteColumnType);
		}

		public override TListColumns GetDefaultListColumns()
		{
			return new TNoteListColumns();
		}

		public TNoteListMan(TGEDCOMTree aTree) : base(aTree)
		{
		}
	}
}
