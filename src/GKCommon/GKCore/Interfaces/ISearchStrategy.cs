using System.Collections.Generic;

namespace GKCore.Interfaces
{
	public interface ISearchResult
	{
	}

	public interface ISearchStrategy
	{
		IList<ISearchResult> FindAll();
		ISearchResult FindNext();
		ISearchResult FindPrev();
		bool HasResults();
	}
}
