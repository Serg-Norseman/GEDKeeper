using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

using GKSys;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKUI.Lists
{
	public enum TPersonColumnType : byte
	{
		pctPatriarch,
		pctName,
		pctNick,
		pctSex,
		pctBirthDate,
		pctDeathDate,
		pctBirthPlace,
		pctDeathPlace,
		pctResidence,
		pctAge,
		pctLifeExpectancy,
		pctDaysForBirth,
		pctGroups,
		pctReligion,
		pctNationality,
		pctEducation,
		pctOccupation,
		pctCaste,
		pctMili,
		pctMiliInd,
		pctMiliDis,
		pctMiliRank,
		pctChangeDate,
		pctBookmark,
		pctTitle,

		pctLast = pctTitle
	}

	public struct TPersonColumnProps
	{
		public TPersonColumnType colType;
		public byte colSubType;
		public bool colActive;

		public TPersonColumnProps(TPersonColumnType colType, bool colActive) {
			this.colType = colType;
			this.colSubType = 0;
			this.colActive = colActive;
		}
	}

	public sealed class TIndividualListColumns
	{
		private List<TPersonColumnProps> FColumns;

		private static readonly TPersonColumnProps[] DefPersonColumns;

		public int Count
		{
			get { return FColumns.Count; }
		}

		public TPersonColumnProps this[int index]
		{
			get { return FColumns[index]; }
			set { FColumns[index] = value; }
		}

		static TIndividualListColumns()
		{
			TPersonColumnProps[] array1 = new TPersonColumnProps[25];
			array1[0] = new TPersonColumnProps(TPersonColumnType.pctPatriarch, true);
			array1[1] = new TPersonColumnProps(TPersonColumnType.pctName, true);
			array1[2] = new TPersonColumnProps(TPersonColumnType.pctNick, false);
			array1[3] = new TPersonColumnProps(TPersonColumnType.pctSex, true);
			array1[4] = new TPersonColumnProps(TPersonColumnType.pctBirthDate, true);
			array1[5] = new TPersonColumnProps(TPersonColumnType.pctDeathDate, true);
			array1[6] = new TPersonColumnProps(TPersonColumnType.pctBirthPlace, true);
			array1[7] = new TPersonColumnProps(TPersonColumnType.pctDeathPlace, true);
			array1[8] = new TPersonColumnProps(TPersonColumnType.pctResidence, true);
			array1[9] = new TPersonColumnProps(TPersonColumnType.pctAge, true);
			array1[10] = new TPersonColumnProps(TPersonColumnType.pctLifeExpectancy, true);
			array1[11] = new TPersonColumnProps(TPersonColumnType.pctDaysForBirth, true);
			array1[12] = new TPersonColumnProps(TPersonColumnType.pctGroups, true);
			array1[13] = new TPersonColumnProps(TPersonColumnType.pctReligion, false);
			array1[14] = new TPersonColumnProps(TPersonColumnType.pctNationality, false);
			array1[15] = new TPersonColumnProps(TPersonColumnType.pctEducation, false);
			array1[16] = new TPersonColumnProps(TPersonColumnType.pctOccupation, false);
			array1[17] = new TPersonColumnProps(TPersonColumnType.pctCaste, false);
			array1[18] = new TPersonColumnProps(TPersonColumnType.pctMili, false);
			array1[19] = new TPersonColumnProps(TPersonColumnType.pctMiliInd, false);
			array1[20] = new TPersonColumnProps(TPersonColumnType.pctMiliDis, false);
			array1[21] = new TPersonColumnProps(TPersonColumnType.pctMiliRank, false);
			array1[22] = new TPersonColumnProps(TPersonColumnType.pctChangeDate, true);
			array1[23] = new TPersonColumnProps(TPersonColumnType.pctBookmark, true);
			array1[24] = new TPersonColumnProps(TPersonColumnType.pctTitle, true);
			DefPersonColumns = array1;
		}

		public TIndividualListColumns()
		{
			FColumns = new List<TPersonColumnProps>();
			for (int i = 0; i <= (int)TPersonColumnType.pctLast; i++)
			{
				FColumns.Add(new TPersonColumnProps());
			}
		}

		public void Clear()
		{
			FColumns.Clear();
		}

		public void SetDefaults()
		{
			for (int i = 0; i <= (int)TPersonColumnType.pctLast; i++)
			{
				FColumns[i] = DefPersonColumns[i];
			}
		}

		public void CopyTo(TIndividualListColumns columns)
		{
			for (int i = 0; i <= (int)TPersonColumnType.pctLast; i++)
			{
				TPersonColumnProps col = this.FColumns[i];
				columns[i] = col;
			}
		}

		public void LoadFromFile([In] IniFile aIniFile)
		{
			for (int i = 0; i <= (int)TPersonColumnType.pctLast; i++)
			{
				TPersonColumnProps def_col = DefPersonColumns[i];
				TPersonColumnProps col = this.FColumns[i];
				col.colType = (TPersonColumnType)aIniFile.ReadInteger("PersonsColumns", "ColType_" + i.ToString(), (int)def_col.colType);
				col.colActive = aIniFile.ReadBool("PersonsColumns", "ColActive_" + i.ToString(), def_col.colActive);
				this.FColumns[i] = col;
			}
		}

		public void SaveToFile([In] IniFile aIniFile)
		{
			for (int i = 0; i <= (int)TPersonColumnType.pctLast; i++)
			{
				aIniFile.WriteInteger("PersonsColumns", "ColType_" + i.ToString(), (int)((sbyte)this.FColumns[i].colType));
				aIniFile.WriteBool("PersonsColumns", "ColActive_" + i.ToString(), this.FColumns[i].colActive);
			}
		}
	}
}
