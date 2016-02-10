using System;
using System.Collections;
using System.Collections.Specialized;

namespace GKCommon
{
    [Serializable]
	public sealed class ValuesCollection : NameObjectCollectionBase
	{
        public ValuesCollection()
		{
		}

        private static string[] GetAsStringArray(ArrayList list)
		{
            if (list == null)
			{
				return null;
			}
            
            int num = list.Count;
            string[] array = new string[num];
			list.CopyTo(0, array, 0, num);
			return array;
		}

        public void Clear()
		{
			base.BaseClear();
		}

        public bool HasKeys()
		{
			return base.BaseHasKeys();
		}

        public void Add(string name, string value, bool excludeDuplicates)
		{
			ArrayList arrayList = (ArrayList)base.BaseGet(name);

            if (arrayList == null) {
				arrayList = new ArrayList(1);
				base.BaseAdd(name, arrayList);
			}

            if (value == null) return;

            if (!excludeDuplicates)
            {
                arrayList.Add(value);
            }
            else
            {
                if (!arrayList.Contains(value)) arrayList.Add(value);
            }
		}

        public string[] GetValues(string name)
		{
			ArrayList list = (ArrayList)base.BaseGet(name);
			return ValuesCollection.GetAsStringArray(list);
		}

        public void Remove(string name)
		{
			base.BaseRemove(name);
		}
	}
}
