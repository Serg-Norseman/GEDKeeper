using GKSys;
using System;
using System.ComponentModel;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TXRefReplaceMap
	{
		[StructLayout(LayoutKind.Auto)]
		public struct TXRefRec
		{
			public TGEDCOMRecord Rec;
			public string OldXRef;
			public string NewXRef;
		}

		internal TXRefReplaceMap.TXRefRec[] FList;

		public int Count
		{
			get	{ return this.GetCount(); }
		}

		public TXRefReplaceMap.TXRefRec /*Records*/ this[int Index]
		{
			get { return this.GetRecord(Index); }
		}

		internal int GetCount()
		{
			TXRefReplaceMap.TXRefRec[] fList = this.FList;
			return (fList != null) ? fList.Length : 0;
		}

		internal TXRefReplaceMap.TXRefRec GetRecord(int Index)
		{
			return this.FList[Index];
		}

		public void AddXRef(TGEDCOMRecord rec, string oldXRef, string newXRef)
		{
			TXRefReplaceMap.TXRefRec[] fList = this.FList;
			int Len = (fList != null) ? fList.Length : 0;
			TXRefReplaceMap.TXRefRec[] arg_1C_0 = this.FList;
			int num = Len + 1;
			TXRefReplaceMap.TXRefRec[] array = arg_1C_0;
			int arg_24_0;
			if ((arg_24_0 = num) < 0)
			{
				arg_24_0 = 0;
			}
			TXRefReplaceMap.TXRefRec[] array2;
			TXRefReplaceMap.TXRefRec[] expr_29 = array2 = new TXRefReplaceMap.TXRefRec[arg_24_0];
			if (num > 0 && array != null)
			{
				int num2;
				if ((num2 = array.Length) > num)
				{
					num2 = num;
				}
				if (num2 > 0)
				{
					Array.Copy(array, array2, num2);
				}
			}
			this.FList = expr_29;
			this.FList[Len].Rec = rec;
			this.FList[Len].OldXRef = oldXRef;
			this.FList[Len].NewXRef = newXRef;
		}

		public string FindNewXRef(string oldXRef)
		{
			string Result = oldXRef;
			int arg_16_0 = 0;
			TXRefReplaceMap.TXRefRec[] fList = this.FList;
			int num = ((fList != null) ? fList.Length : 0) - 1;
			int i = arg_16_0;
			if (num >= i)
			{
				num++;
				while (BDSSystem.WStrCmp(TGEDCOMObject.CleanXRef(this.FList[i].OldXRef), TGEDCOMObject.CleanXRef(oldXRef)) != 0)
				{
					i++;
					if (i == num)
					{
						return Result;
					}
				}
				Result = this.FList[i].NewXRef;
			}
			return Result;
		}

		public void Free()
		{
			TObjectHelper.Free(this);
		}
	}
}
