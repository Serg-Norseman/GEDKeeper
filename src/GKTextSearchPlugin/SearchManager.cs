using System;
using System.Collections.Generic;
using System.IO;

using ExtUtils;
using GedCom551;
using GKCore.Interfaces;
using Xapian;

/// <summary>
/// Localization: clean
/// </summary>

namespace GKTextSearchPlugin
{
	public class SearchManager
	{
		private readonly object xdb_lock;
		private Plugin fPlugin;

		public SearchManager(Plugin plugin)
		{
			this.fPlugin = plugin;
            this.xdb_lock = new object();
		}

		#region Private methods
		
		private static string GetSign(IBase aBase)
		{
			return Path.GetFileNameWithoutExtension(aBase.Tree.FileName);
		}

		private static bool IsIndexedRecord(TGEDCOMRecord rec)
		{
			return !((rec is TGEDCOMLocationRecord || rec is TGEDCOMGroupRecord));
		}

		private static void SetDBLastChange(IBase aBase, WritableDatabase database)
		{
			string db_lastchange = aBase.Tree.Header.TransmissionDateTime.ToString("yyyy.MM.dd HH:mm:ss", null);
			database.SetMetadata(GetSign(aBase), db_lastchange);
		}

		private string GetXDBFolder()
		{
			string xdb_dir = fPlugin.Host.GetAppDataPath() + "xdb";
			if (!Directory.Exists(xdb_dir)) Directory.CreateDirectory(xdb_dir);
			return xdb_dir;
		}

		private static uint FindDocId(IBase aBase, WritableDatabase database, string xref)
		{
			uint result;

			string key = "Q" + GetSign(aBase) + "_" + xref;

			PostingIterator p = database.PostListBegin(key);
			if (p == database.PostListEnd(key)) {
				result = 0; // 0 - is invalid docid (see XapianManual)
			} else {
				result = p.GetDocId();
			}

			return result;
		}

		private static void SetDocumentContext(IBase aBase, Document doc, TermGenerator indexer, TGEDCOMRecord rec)
		{
			StringList ctx = aBase.GetRecordContent(rec);
			string rec_lastchange = rec.ChangeDate.ToString();
			string base_sign = GetSign(aBase);

			doc.SetData(rec.XRef);							// not edit: for link from search results to gedcom-base
			doc.AddTerm("Q" + base_sign + "_" + rec.XRef);	// not edit: specific db_rec_id - for FindDocId()
			doc.AddValue(0, rec_lastchange);				// not edit: for update check
			doc.AddBooleanTerm("GDB" + base_sign);			// not edit: for filtering by database in Search()

			indexer.SetDocument(doc);
			indexer.IndexText(ctx.Text);
		}

		private static void ReindexRecord(IBase aBase, WritableDatabase database, TermGenerator indexer, TGEDCOMRecord record)
		{
			uint docid = FindDocId(aBase, database, record.XRef);

			if (docid != 0) {
				// проверка на необходимость обновления
				string rec_lastchange = record.ChangeDate.ToString();

				Document cur_doc = database.GetDocument(docid);
				string doc_lastchange = cur_doc.GetValue(0);

				// обновление записи
				if (!string.Equals(rec_lastchange, doc_lastchange)) {
					using (Document doc = new Document())
					{
						SetDocumentContext(aBase, doc, indexer, record);
						database.ReplaceDocument(docid, doc);
					}
				}
			} else {
				// чистое добавление
				using (Document doc = new Document())
				{
					SetDocumentContext(aBase, doc, indexer, record);
					database.AddDocument(doc);
				}
			}
		}

		#endregion

		public void ReindexBase(IBase aBase)
		{
		    if (aBase == null) return;

			try
			{
				lock (xdb_lock)
        		{
					using (WritableDatabase database = new WritableDatabase(GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
					using (TermGenerator indexer = new TermGenerator())
					using (Stem stemmer = new Xapian.Stem("russian"))
					{
						indexer.SetStemmer(stemmer);

						aBase.ProgressInit(fPlugin.LangMan.LS(TLS.LSID_SearchIndexRefreshing), aBase.Tree.RecordsCount);
						int num = aBase.Tree.RecordsCount - 1;
						for (int i = 0; i <= num; i++)
						{
							TGEDCOMRecord record = aBase.Tree[i];
							if (IsIndexedRecord(record)) ReindexRecord(aBase, database, indexer, record);

							aBase.ProgressStep();
						}
						aBase.ProgressDone();

						SetDBLastChange(aBase, database);
					}
				}
			}
			catch (Exception ex)
			{
				aBase.Host.LogWrite("SearchManager.ReindexBase(): " + ex.Message);
			}
		}

		public void UpdateRecord(IBase aBase, TGEDCOMRecord record)
		{
			if (record == null || !IsIndexedRecord(record)) return;

			try
			{
				lock (xdb_lock)
        		{
					using (WritableDatabase database = new WritableDatabase(GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
					using (TermGenerator indexer = new TermGenerator())
					using (Stem stemmer = new Xapian.Stem("russian"))
					{
						indexer.SetStemmer(stemmer);

						ReindexRecord(aBase, database, indexer, record);
						SetDBLastChange(aBase, database);
					}
				}
			}
			catch (Exception ex)
			{
				aBase.Host.LogWrite("SearchManager.UpdateRecord(): " + ex.Message);
			}
		}

		public void DeleteRecord(IBase aBase, string xref)
		{
			try
			{
				lock (xdb_lock)
        		{
					using (WritableDatabase database = new WritableDatabase(GetXDBFolder(), Xapian.Xapian.DB_CREATE_OR_OPEN))
					{
						uint docid = FindDocId(aBase, database, xref);
						if (docid != 0) {
							database.DeleteDocument(docid);
							SetDBLastChange(aBase, database);
						}
					}
				}
			}
			catch (Exception ex)
			{
				aBase.Host.LogWrite("SearchManager.DeleteRecord(): " + ex.Message);
			}
		}

		public class SearchEntry
		{
			public string XRef;
			public uint Rank;
			public int Percent;
		}

		public List<SearchEntry> Search(IBase aBase, string searchText)
		{
            const uint flags = (uint)(QueryParser.feature_flag.FLAG_PARTIAL | QueryParser.feature_flag.FLAG_WILDCARD |
                                      QueryParser.feature_flag.FLAG_PHRASE | QueryParser.feature_flag.FLAG_BOOLEAN |
                                      QueryParser.feature_flag.FLAG_LOVEHATE);
            
            List<SearchEntry> res = new List<SearchEntry>();

			try
			{
				lock (xdb_lock)
				{
					using (Database database = new Database(GetXDBFolder()))
					using (Enquire enquire = new Enquire(database))
					using (Stem stemmer = new Stem("russian"))
					using (QueryParser qp = new QueryParser())
					{
						qp.SetStemmer(stemmer);
						qp.SetDatabase(database);
						qp.SetDefaultOp(Query.op.OP_AND);
						qp.SetStemmingStrategy(QueryParser.stem_strategy.STEM_SOME);

						string qs = searchText + " ged:" + GetSign(aBase);
						qp.AddBooleanPrefix("ged", "GDB");

						using (Query query = qp.ParseQuery(qs, flags))
						{
							enquire.SetQuery(query);

							using (MSet matches = enquire.GetMSet(0, 100))
							{
								MSetIterator m = matches.Begin();
								while (m != matches.End())
								{
									try
									{
										SearchEntry entry = new SearchEntry();
										entry.XRef = m.GetDocument().GetData();
										entry.Rank = m.GetRank()+1;
										entry.Percent = m.GetPercent();
										res.Add(entry);
									}
									catch (Exception ex)
									{
										aBase.Host.LogWrite("SearchManager.Search(): " + ex.Message);
									}

									m = m.Next();
								}
							}
						}
					}
				}
			}
			catch (Exception ex)
			{
				aBase.Host.LogWrite("SearchManager.Search(): " + ex.Message);
			}

			return res;
		}

	}
}
