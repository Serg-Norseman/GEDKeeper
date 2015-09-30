using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;

namespace GKCommon.GEDCOM
{
	public sealed class GEDCOMList<T> : IDisposable, IEnumerable where T : GEDCOMObject
	{
		#region ListEnumerator

		private struct GEDCOMListEnumerator : IGEDCOMListEnumerator
		{
			private readonly GEDCOMList<T> fOwnList;
			private int fIndex;
			private int fSize;

			public GEDCOMListEnumerator(GEDCOMList<T> list)
			{
				this.fOwnList = list;

				this.fIndex = -1;

				List<T> dataList = list.fDataList;
				this.fSize = ((dataList == null) ? 0 : dataList.Count);
			}

			void IEnumerator.Reset()
			{
				this.fIndex = -1;

				List<T> dataList = this.fOwnList.fDataList;
				this.fSize = ((dataList == null) ? 0 : dataList.Count);
			}

			bool IEnumerator.MoveNext()
			{
				this.fIndex++;
				return (this.fIndex < this.fSize);
			}

			object IEnumerator.Current
			{
				get { return this.fOwnList.fDataList[this.fIndex]; }
			}

			GEDCOMObject IGEDCOMListEnumerator.Owner
			{
				get { return this.fOwnList.fOwner; }
			}
		}

		#endregion

		
		private List<T> fDataList; // lazy implementation
		private readonly GEDCOMObject fOwner;
		private bool fDisposed;

		public int Count
		{
			get {
				return ((this.fDataList == null) ? 0 : this.fDataList.Count);
			}
		}

		public T this[int index]
		{
			get {
				return ((this.fDataList == null) ? default(T) : this.fDataList[index]);
			}
		}

		public GEDCOMObject Owner
		{
			get {
				return this.fOwner;
			}
		}
		
		public GEDCOMList(GEDCOMObject owner)
		{
		    this.fOwner = owner;
            this.fDataList = null;
        }

	    public void Dispose()
		{
			if (!this.fDisposed)
			{
				this.Clear();
				//this.fList.Free(); isnot IDisposable
				this.fDisposed = true;
			}
		}

		public void ForEach(Action<T> action)
		{
			if (action == null) {
				throw new ArgumentNullException("action");
			}

			if (this.fDataList == null) return;

			int num = this.fDataList.Count;
			for (int i = 0; i < num; i++) {
				action(this.fDataList[i]);
			}
		}

		public IGEDCOMListEnumerator GetEnumerator()
		{
			return new GEDCOMListEnumerator(this);
		}

		IEnumerator IEnumerable.GetEnumerator()
		{
			return new GEDCOMListEnumerator(this);
		}
		
		public T Add(T item)
		{
			if (item != null)
			{
				if (this.fDataList == null)
				{
					this.fDataList = new List<T>();
				}

				this.fDataList.Add(item);
			}

			return item;
		}

		public void Clear()
		{
			if (this.fDataList == null) return;

			for (int i = this.fDataList.Count - 1; i >= 0; i--)
			{
				(this.fDataList[i] as GEDCOMObject).Dispose();
			}
			this.fDataList.Clear();
		}

		public void Delete(int index)
		{
		    if (this.fDataList == null) return;
		    
            this.fDataList[index].Dispose();
		    this.fDataList.RemoveAt(index);
		}

		public void DeleteObject(T item)
		{
			if (this.fDataList == null) return;

			int idx = this.fDataList.IndexOf(item);
			if (idx >= 0) {
				this.Delete(idx);
			}
		}

		public void Exchange(int index1, int index2)
		{
			if (this.fDataList == null) return;

			if (index1 >= 0 && index1 < this.fDataList.Count && index2 >= 0 && index2 < this.fDataList.Count)
			{
				T tmp = this.fDataList[index1];
				this.fDataList[index1] = this.fDataList[index2];
				this.fDataList[index2] = tmp;
			}
		}

		public T Extract(int index)
		{
			if (this.fDataList != null) {
				T result = this.fDataList[index];
				this.fDataList.RemoveAt(index);
				return result;
			} else {
				return default(T);
			}
		}

		public int IndexOfObject(T item)
		{
			return (this.fDataList == null) ? -1 : this.fDataList.IndexOf(item);
		}

		public void SaveToStream(StreamWriter stream)
		{
			if (this.fDataList == null) return;

			int num = this.fDataList.Count;
			for (int i = 0; i < num; i++) {
				T item = this.fDataList[i];
				if (item is GEDCOMTag) {
					(item as GEDCOMTag).SaveToStream(stream);
				}
			}
		}

		public void ReplaceXRefs(XRefReplacer map)
		{
			if (this.fDataList == null) return;

			int num = this.fDataList.Count;
			for (int i = 0; i < num; i++) {
				T item = this.fDataList[i];
				if (item is GEDCOMTag) {
					(item as GEDCOMTag).ReplaceXRefs(map);
				}
			}
		}

		public void ResetOwner(GEDCOMTree newOwner)
		{
			if (this.fDataList == null) return;

			int num = this.fDataList.Count;
			for (int i = 0; i < num; i++) {
				(this.fDataList[i] as GEDCOMTag).ResetOwner(newOwner);
			}

			//this._owner = newOwner;
		}

		public void Pack()
		{
			if (this.fDataList == null) return;

			for (int i = this.fDataList.Count - 1; i >= 0; i--) {
				T item = this.fDataList[i];
				if (item is GEDCOMTag) {
					GEDCOMTag tag = item as GEDCOMTag;
					tag.Pack();
					if (tag.IsEmpty() && tag.IsEmptySkip()) {
						this.Delete(i);
					}
				}
			}
		}
	}
}
