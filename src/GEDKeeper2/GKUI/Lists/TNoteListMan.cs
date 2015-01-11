using System;

using GedCom551;
using GKCore;
using GKCore.Interfaces;

/// <summary>
/// 
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
			this.AddStatic(LangMan.LS(LSID.LSID_Note), TDataType.dtString, 400, true);
			this.AddStatic(LangMan.LS(LSID.LSID_Changed), TDataType.dtDateTime, 150, true);
		}

		public TNoteListColumns() : base()
		{
			InitData(typeof(TNoteColumnType));
		}
	}

	public sealed class TNoteListMan : TListManager
	{
		private TGEDCOMNoteRecord FRec;

		public override bool CheckFilter(ShieldState aShieldState)
		{
			bool res = base.CheckNewFilter();
			return res;
		}

		public override void Fetch(TGEDCOMRecord aRec)
		{
			this.FRec = (aRec as TGEDCOMNoteRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype)
		{
			switch (colType) {
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
