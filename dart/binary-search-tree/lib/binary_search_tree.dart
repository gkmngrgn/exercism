class Node {
  Node left;
  Node right;
  int value;
  String data;

  Node(this.data) : value = int.parse(data);
}

class BinarySearchTree {
  Node root;
  List<String> get sortedData => getSortedData(root, []);

  BinarySearchTree(String data) : root = Node(data);

  void insert(String data) {
    root = addNode(data, root);
  }

  Node addNode(String data, Node currentNode) {
    var node = Node(data);
    if (currentNode == null) {
      return node;
    }
    if (currentNode.value >= node.value) {
      currentNode.left = addNode(data, currentNode.left);
    } else if (currentNode.value < node.value) {
      currentNode.right = addNode(data, currentNode.right);
    }
    return currentNode;
  }

  List<String> getSortedData(Node currentNode, List<String> dataList) {
    if (currentNode.left != null) {
      dataList = getSortedData(currentNode.left, dataList);
    }
    dataList.add(currentNode.data);
    if (currentNode.right != null) {
      dataList = getSortedData(currentNode.right, dataList);
    }
    return dataList;
  }
}
