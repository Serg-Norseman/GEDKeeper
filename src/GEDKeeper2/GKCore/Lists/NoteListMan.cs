using System;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Types;

namespace GKCore.Lists
{
	/// <summary>
	/// 
	/// </summary>
	public enum NoteColumnType
	{
		nctText,
		nctChangeDate
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class NoteListColumns : ListColumns
	{
		protected override void InitColumnStatics()
		{
			this.AddStatic(LSID.LSID_Note, TDataType.dtString, 400, true);
			this.AddStatic(LSID.LSID_Changed, TDataType.dtDateTime, 150, true);
		}

		public NoteListColumns() : base()
		{
			InitData(typeof(NoteColumnType));
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public sealed class NoteListMan : ListManager
	{
		private GEDCOMNoteRecord fRec;

		public override bool CheckFilter(ShieldState shieldState)
		{
			bool res = base.CheckCommonFilter();
			return res;
		}

		public override void Fetch(GEDCOMRecord aRec)
		{
			this.fRec = (aRec as GEDCOMNoteRecord);
		}

		protected override object GetColumnValueEx(int colType, int colSubtype, bool isVisible)
		{
			switch (colType) {
				case 0:
					{
						string st;
						if (this.fRec.Note.Count > 0)
						{
							st = this.fRec.Note[0].Trim();
							if (st == "" && this.fRec.Note.Count > 1)
							{
								st = this.fRec.Note[1].Trim();
							}
						}
						else
						{
							st = "";
						}
						return st;
					}
				case 1:
					return this.fRec.ChangeDate.ChangeDateTime;
				default:
					return null;
			}
		}

		public override Type GetColumnsEnum()
		{
			return typeof(NoteColumnType);
		}

		protected override ListColumns GetDefaultListColumns()
		{
			return new NoteListColumns();
		}

		public NoteListMan(GEDCOMTree tree) : base(tree)
		{
		}
	}
}
