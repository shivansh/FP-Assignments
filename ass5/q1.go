package main

import "fmt"

type Node struct {
	key   int
	left  *Node
	right *Node
}

// Max returns maximum of two integers.
func Max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// Insert inserts a key k into the BST.
func Insert(root *Node, k int) *Node {
	if root == nil {
		root = &Node{k, nil, nil}
		return root
	}
	if k <= root.key {
		root.left = Insert(root.left, k)
	} else {
		root.right = Insert(root.right, k)
	}
	return root
}

// Diameter returns the largest distance between any two nodes in a BST.
// At each recursive step, the following is evaluated -
//	* The diameter is calculated as the maximum amongst diameter of left
//	  subtree, right subtree and longest path going through current node.
//	  The longest path through current node is (leftDepth+rightDepth+1).
//	* Maximum depth of the subtree rooted at the current node (maxDepth).
func Diameter(root *Node) (diam int, maxDepth int) {
	if root == nil {
		return 0, 0
	}
	d, leftPath := Diameter(root.left)
	diam = Max(diam, d)
	d, rightPath := Diameter(root.right)
	diam = Max(Max(diam, d), leftPath+rightPath+1)
	return diam, Max(leftPath, rightPath) + 1
}

func main() {
	root := &Node{5, nil, nil}
	root = Insert(root, 2)
	root = Insert(root, 3)
	root = Insert(root, 1)
	root = Insert(root, 4)
	root = Insert(root, 6)
	root = Insert(root, 7)
	diam, _ := Diameter(root)
	fmt.Println(diam)
}
