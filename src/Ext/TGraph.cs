using System;

/// <summary>
/// Localization: clean
/// </summary>

namespace Ext.Utils
{
	public class TGraph : IDisposable
	{
		public class TGraphNode
		{
			public object ExtObj;
			public TGraph.TGraphNode NextNode;
			public TGraph.TGraphLink LinksOut;
			public TGraph.TGraphLink LinkIn;
			public TGraph.TNodeStatus Status;
			public int Dist;

			public void Free()
			{
				SysUtils.Free(this);
			}
		}

		public class TGraphLink
		{
			public TGraph.TGraphNode Node1;
			public TGraph.TGraphNode Node2;
			public int Cost;
			public int ExtData;
			public bool InTree;
			public TGraph.TGraphLink NextLink;

			public void Free()
			{
				SysUtils.Free(this);
			}
		}

		public class TCandidate
		{
			public TGraph.TGraphNode Node;
			public TGraph.TCandidate NextCandidate;

			public void Free()
			{
				SysUtils.Free(this);
			}
		}

		public enum TNodeStatus : byte
		{
			nsNotInList,
			nsWasInList,
			nsNowInList
		}

		public TGraph.TGraphNode NodeList;
		protected bool Disposed_;

		public TGraph()
		{
			this.NodeList = null;
		}

		public void Dispose()
		{
			if (!this.Disposed_)
			{
				this.Clear();
				this.Disposed_ = true;
			}
		}

		public void Clear()
		{
			TGraph.TGraphNode node = this.NodeList;
			if (node != null)
			{
				do
				{
					TGraph.TGraphLink link = node.LinksOut;
					if (link != null)
					{
						do
						{
							TGraph.TGraphLink next_link = link.NextLink;
							link.Free();
							link = next_link;
						}
						while (link != null);
					}
					TGraph.TGraphNode next_node = node.NextNode;
					node.Free();
					node = next_node;
				}
				while (node != null);
			}
			this.NodeList = null;
		}

		public void CreateLink(TGraph.TGraphNode Node_1, TGraph.TGraphNode Node_2, int Cost, int ExtData1, int ExtData2)
		{
			if (Node_1 != null && Node_2 != null && Node_1 != Node_2)
			{
				TGraph.TGraphLink link = new TGraph.TGraphLink();
				TGraph.TGraphLink link2 = new TGraph.TGraphLink();
				link.Cost = Cost;
				link.Node1 = Node_1;
				link.Node2 = Node_2;
				link.NextLink = null;
				link.ExtData = ExtData1;
				link2.Cost = Cost;
				link2.Node1 = Node_2;
				link2.Node2 = Node_1;
				link2.NextLink = null;
				link2.ExtData = ExtData2;
				link.NextLink = Node_1.LinksOut;
				Node_1.LinksOut = link;
				link2.NextLink = Node_2.LinksOut;
				Node_2.LinksOut = link2;
				this.ResetPathTree();
			}
		}

		public TGraph.TGraphNode CreateNode(/*TGraph.TGraphNode aNodeClass, */object aExtObj)
		{
			TGraph.TGraphNode node = new TGraphNode();
			node.LinksOut = null;
			node.ExtObj = aExtObj;
			node.NextNode = this.NodeList;
			this.NodeList = node;
			TGraph.TGraphNode Result = node;
			this.ResetPathTree();
			return Result;
		}

		public void DeleteLink(TGraph.TGraphNode n1, TGraph.TGraphNode n2)
		{
			if (n1 != null && n2 != null)
			{
				this.RemoveLinkFromNode(n1, n2);
				this.RemoveLinkFromNode(n2, n1);
				this.ResetPathTree();
			}
		}

		public void DeleteNode(TGraph.TGraphNode target)
		{
			if (target != null)
			{
				while (target.LinksOut != null)
				{
					this.DeleteLink(target, target.LinksOut.Node2);
				}
				TGraph.TGraphNode node = null;
				TGraph.TGraphNode next_node = this.NodeList;
				if (next_node != null)
				{
					while (!object.Equals(next_node, target))
					{
						node = next_node;
						next_node = node.NextNode;
						if (next_node == null)
						{
							break;
						}
					}
				}
				if (next_node != null)
				{
					if (node == null)
					{
						this.NodeList = target.NextNode;
					}
					else
					{
						node.NextNode = target.NextNode;
					}
					target.Free();
					this.ResetPathTree();
				}
			}
		}

		public void FindPathTree(TGraph.TGraphNode root)
		{
			if (root != null)
			{
				this.ResetPathTree();
				root.Dist = 0;
				root.LinkIn = null;
				root.Status = TGraph.TNodeStatus.nsNowInList;
				TGraph.TCandidate new_candidate = new TGraph.TCandidate();
				TGraph.TCandidate top_candidate = new_candidate;
				new_candidate.NextCandidate = null;
				new_candidate.Node = root;
				TGraph.TGraphNode to_node;
				if (top_candidate != null)
				{
					do
					{
						TGraph.TGraphNode node = top_candidate.Node;
						new_candidate = top_candidate;
						top_candidate = top_candidate.NextCandidate;
						new_candidate.Free();
						int node_dist = node.Dist;
						node.Status = TGraph.TNodeStatus.nsNotInList;
						TGraph.TGraphLink link = node.LinksOut;
						if (link != null)
						{
							do
							{
								to_node = link.Node2;
								int new_dist = node_dist + link.Cost;
								if (new_dist < to_node.Dist)
								{
									to_node.LinkIn = link;
									to_node.Dist = new_dist;
									if (to_node.Status == TGraph.TNodeStatus.nsNotInList)
									{
										new_candidate = new TGraph.TCandidate();
										new_candidate.NextCandidate = top_candidate;
										top_candidate = new_candidate;
										new_candidate.Node = to_node;
										to_node.Status = TGraph.TNodeStatus.nsNowInList;
									}
								}
								link = link.NextLink;
							}
							while (link != null);
						}
					}
					while (top_candidate != null);
				}
				to_node = this.NodeList;
				if (to_node != null)
				{
					do
					{
						TGraph.TGraphLink link = to_node.LinkIn;
						if (link != null)
						{
							link.InTree = true;
							TGraph.TGraphNode node = link.Node1;
							link = to_node.LinksOut;
							if (link != null)
							{
								while (!object.Equals(link.Node2, node))
								{
									link = link.NextLink;
									if (link == null)
									{
										break;
									}
								}
							}
							if (link != null)
							{
								link.InTree = true;
							}
						}
						to_node = to_node.NextNode;
					}
					while (to_node != null);
				}
			}
		}

		public void RemoveLinkFromNode(TGraph.TGraphNode n1, TGraph.TGraphNode n2)
		{
			TGraph.TGraphLink link = null;
			TGraph.TGraphLink next_link = n1.LinksOut;
			if (next_link != null)
			{
				while (!object.Equals(next_link.Node2, n2))
				{
					link = next_link;
					next_link = link.NextLink;
					if (next_link == null)
					{
						break;
					}
				}
			}
			if (next_link != null)
			{
				if (link == null)
				{
					n1.LinksOut = next_link.NextLink;
				}
				else
				{
					link.NextLink = next_link.NextLink;
				}
				next_link.Free();
			}
		}

		public void ResetPathTree()
		{
			TGraph.TGraphNode node = this.NodeList;
			if (node != null)
			{
				do
				{
					node.Status = TGraph.TNodeStatus.nsNotInList;
					node.Dist = 32767;
					node.LinkIn = null;
					TGraph.TGraphLink link = node.LinksOut;
					if (link != null)
					{
						do
						{
							link.InTree = false;
							link = link.NextLink;
						}
						while (link != null);
					}
					node = node.NextNode;
				}
				while (node != null);
			}
		}

		public void Free()
		{
			SysUtils.Free(this);
		}
	}
}
