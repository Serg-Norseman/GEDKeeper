using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Cyotek.Windows.Forms
{
	//[TypeConverter(typeof(ZoomLevelCollectionConverter))]
	public class ZoomLevelCollection : IList<int>
	{
		#region Constructors

		public ZoomLevelCollection()
		{
			this.List = new SortedList<int, int>();
		}

		public ZoomLevelCollection(IEnumerable<int> collection)
			: this()
		{
			if (collection == null)
				throw new ArgumentNullException("collection");

			this.AddRange(collection);
		}

		#endregion

		#region Class Properties

		public static ZoomLevelCollection Default
		{
			get { return new ZoomLevelCollection(new[] {7, 10, 15, 20, 25, 30, 50, 70, 100, 150, 200, 300, 400, 500, 600, 700, 800, 1200, 1600}); }
		}

		#endregion

		#region Properties

		public int Count
		{
			get { return this.List.Count; }
		}

		public bool IsReadOnly
		{
			get { return false; }
		}

		public int this[int index]
		{
			get { return this.List.Values[index]; }
			set
			{
				this.List.RemoveAt(index);
				this.Add(value);
			}
		}

		protected SortedList<int, int> List { get; set; }

		#endregion

		#region Members

		public void Add(int item)
		{
			this.List.Add(item, item);
		}

		public void AddRange(IEnumerable<int> collection)
		{
			if (collection == null)
				throw new ArgumentNullException("collection");

			foreach (int value in collection)
				this.Add(value);
		}

		public void Clear()
		{
			this.List.Clear();
		}

		public bool Contains(int item)
		{
			return this.List.ContainsKey(item);
		}

		public void CopyTo(int[] array, int arrayIndex)
		{
			if (this.Count != 0)
				Array.Copy(this.List.Values.ToArray(), 0, array, arrayIndex, this.Count);
		}

		public int FindNearest(int zoomLevel)
		{
			return this.OrderBy(v => Math.Abs(v - zoomLevel)).First();
		}

		public IEnumerator<int> GetEnumerator()
		{
			return this.List.Values.GetEnumerator();
		}

		public int IndexOf(int item)
		{
			return this.List.IndexOfKey(item);
		}

		public void Insert(int index, int item)
		{
			throw new NotImplementedException();
		}

		public int NextZoom(int zoomLevel)
		{
			int index;

			index = this.IndexOf(this.FindNearest(zoomLevel));
			if (index < this.Count - 1)
				index++;

			return this[index];
		}

		public int PreviousZoom(int zoomLevel)
		{
			int index;

			index = this.IndexOf(this.FindNearest(zoomLevel));
			if (index > 0)
				index--;

			return this[index];
		}

		public bool Remove(int item)
		{
			return this.List.Remove(item);
		}

		public void RemoveAt(int index)
		{
			this.List.RemoveAt(index);
		}

		public int[] ToArray()
		{
			int[] results;

			results = new int[this.Count];
			this.CopyTo(results, 0);

			return results;
		}

		#endregion

		#region IList<int> Members

		IEnumerator IEnumerable.GetEnumerator()
		{
			return this.GetEnumerator();
		}

		#endregion
	}
}
