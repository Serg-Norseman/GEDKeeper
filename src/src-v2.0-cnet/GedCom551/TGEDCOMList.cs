using GKSys;
using System;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace GedCom551
{
	public class TGEDCOMList : IDisposable
	{
		internal TList FList;

		internal TGEDCOMObject FOwner;

		protected internal bool Disposed_;

		[Browsable(false)]
		public int Count
		{
			get
			{
				return this.GetCount();
			}
		}
		[Browsable(false)]
		public TGEDCOMObject this[int Index]
		{
			get
			{
				return this.GetItems(Index);
			}
		}
		internal int GetCount()
		{
			return this.FList.Count;
		}
		internal TGEDCOMObject GetItems(int Index)
		{
			return this.FList[Index] as TGEDCOMObject;
		}

		public TGEDCOMList(TGEDCOMObject AOwner)
		{
			this.FOwner = AOwner;
			this.FList = new TList();
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Clear();
				this.FList.Free();
				this.Disposed_ = true;
			}
		}

		public TGEDCOMObject Add(TGEDCOMObject AObject)
		{
			this.FList.Add(AObject);
			return AObject;
		}
		public void Clear()
		{
			int I = this.Count - 1;
			if (I >= 0)
			{
				do
				{
					(this.FList[I] as TGEDCOMObject).Free();
					I--;
				}
				while (I != -1);
			}
			this.FList.Clear();
		}
		public void Delete(int Index)
		{
			(this.FList[Index] as TGEDCOMObject).Free();
			this.FList.Delete(Index);
		}
		public void DeleteObject(TGEDCOMObject AObject)
		{
			int Index = this.FList.IndexOf(AObject);
			if (Index >= 0)
			{
				this.Delete(Index);
			}
		}
		public void Exchange(int Index1, int Index2)
		{
			if (Index1 >= 0 && Index1 < this.FList.Count && Index2 >= 0 && Index2 < this.FList.Count)
			{
				this.FList.Exchange(Index1, Index2);
			}
		}
		public TGEDCOMObject Extract(int Index)
		{
			TGEDCOMObject Result = this.GetItems(Index);
			this.FList.Delete(Index);
			return Result;
		}
		public int IndexOfObject(TGEDCOMObject AObject)
		{
			return this.FList.IndexOf(AObject);
		}
		public virtual void SaveToStream(StreamWriter AStream)
		{
			int arg_0A_0 = 0;
			int num = this.Count - 1;
			int I = arg_0A_0;
			if (num >= I)
			{
				num++;
				do
				{
					if ((this.FList[I] as TGEDCOMObject) is TGEDCOMCustomTag)
					{
						(this.FList[I] as TGEDCOMCustomTag).SaveToStream(AStream);
					}
					I++;
				}
				while (I != num);
			}
		}
		public void ReplaceXRefs(TXRefReplaceMap aMap)
		{
			int arg_0F_0 = 0;
			int num = this.FList.Count - 1;
			int i = arg_0F_0;
			if (num >= i)
			{
				num++;
				do
				{
					if (this.FList[i] is TGEDCOMCustomTag)
					{
						(this.FList[i] as TGEDCOMCustomTag).ReplaceXRefs(aMap);
					}
					i++;
				}
				while (i != num);
			}
		}
		public void ResetOwner(TGEDCOMObject AOwner)
		{
			int arg_0F_0 = 0;
			int num = this.FList.Count - 1;
			int i = arg_0F_0;
			if (num >= i)
			{
				num++;
				do
				{
					(this.FList[i] as TGEDCOMCustomTag).ResetOwner(AOwner);
					i++;
				}
				while (i != num);
			}
		}
		public void Pack()
		{
			int i = this.FList.Count - 1;
			if (i >= 0)
			{
				do
				{
					if (this.FList[i] is TGEDCOMCustomTag)
					{
						TGEDCOMCustomTag tag = this.FList[i] as TGEDCOMCustomTag;
						tag.Pack();
						if (tag.IsEmpty() && tag.IsEmptySkip())
						{
							this.Delete(i);
						}
					}
					i--;
				}
				while (i != -1);
			}
		}
		public void Free()
		{
			TObjectHelper.Free(this);
		}

	}
}
