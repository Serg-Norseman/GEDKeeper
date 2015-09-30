using System;
using System.Collections.Generic;
using GKCommon.GEDCOM;
using GKCore.Interfaces;

namespace GKCore
{
	public abstract class BaseSearchStrategy : ISearchStrategy
	{
		private String searchPattern;
		private IWorkWindow workWindow;
		private IList<ISearchResult> currentResults;
		private ISearchResult curResult;
		
		protected BaseSearchStrategy(IWorkWindow workWindow, String searchPattern)
		{
			if (searchPattern == null)
				throw new ArgumentNullException("searchPattern");
			this.searchPattern = searchPattern;
			this.workWindow = workWindow;
			this.currentResults = this.FindAll();
		}
		
		public IList<ISearchResult> FindAll()
		{
			return this.workWindow.FindAll(this.searchPattern);
			
			/*foreach (Match result in searchPattern.Matches(document.Text)) {
				int resultEndOffset = result.Length + result.Index;
				if (offset > result.Index || endOffset < resultEndOffset)
					continue;
				if (matchWholeWords && (!IsWordBorder(document, result.Index) || !IsWordBorder(document, resultEndOffset)))
					continue;
				yield return new SearchResult { StartOffset = result.Index, Length = result.Length, Data = result };
			}*/
		}
		
		public bool HasResults()
		{
			return (currentResults != null && currentResults.Count > 0);
		}
		
		public ISearchResult FindNext()
		{
			if (curResult == null) {
				if (currentResults == null) currentResults = this.FindAll();
				
				curResult = GKUtils.FirstOrDefault(currentResults);
			} else {
				int idx = currentResults.IndexOf(curResult) + 1;
				
				if (idx < currentResults.Count) {
					curResult = currentResults[idx];
				} else {
					curResult = null;
				}
			}
			
			return curResult;
		}
		
		public ISearchResult FindPrev()
		{
			if (curResult == null) {
				if (currentResults == null) currentResults = this.FindAll();
				
				curResult = GKUtils.LastOrDefault(currentResults);
			} else {
				int idx = currentResults.IndexOf(curResult) - 1;
				
				if (idx >= 0) {
					curResult = currentResults[idx];
				} else {
					curResult = null;
				}
			}
			
			return curResult;
		}
	}

	public class SearchResult : ISearchResult
	{
		public GEDCOMObject Result { get; private set; }
		
		public SearchResult(GEDCOMObject result)
		{
			this.Result = result;
		}
	}
}
