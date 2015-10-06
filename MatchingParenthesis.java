import java.util.Scanner;
import java.util.Stack;

/**Matching Parenthesis Implementation in Java
Accpets as input a string, and checks for matching parenthesis using Stacks

Todo: Read a C program as input, and check for semantics of the program by matching parenthesis

*/

class Node
{
	private char data;
	private Node next;
	
	public Node()
	{
		data = 0;
		next = null;
	}
	
	public Node(char data)
	{
		this.data = data;
		next = null;
	}
	
	public char getData()
	{
		return data;
	}
	
	public void setData(char data)
	{
		this.data = data;
	}
	
	public Node getNext()
	{
		return next;
	}
	
	public void setNext(Node next)
	{
		this.next = next;
	}
}

class StackImplementation
{
	Node top;
	
	public StackImplementation()
	{
		top = null;
	}
	
	public void push(char data)
	{
		Node node = new Node(data);
		if (top == null)
			node = top;
		else
		{
			node.setNext(top);
			top = node;
		}		
	}
	
	public char pop()
	{
		if (top == null)
			return '\0';
			
		char tempPop = top.getData();
		top = top.getNext();
		return tempPop;
	}
	
	public char peek()
	{
		if (top == null)
			return '\0';
		return top.getData();
	}
	
	public boolean empty()
	{
		if (top == null)
			return true;
		return false;
	}
	
	public boolean checkBraces(String line)
	{	
		if (line == null || line.length() == 0)
			return true;
			
		boolean flag = true;
		
		for (int i=0; i<line.length(); i++)
			{
				if (!empty() && line.charAt(i) == ')')
				{
					if (peek() == '(')
						pop();			
					else
						flag = false;
				}
				
				else if (!empty() && line.charAt(i) == '}')
				{
					if (peek() == '{')
						pop();
					else 
						flag = false;
				}
			
				else if (!empty() && line.charAt(i) == ']')
				{
					if (peek() == '[')
						pop();
					else
						flag = false;
				}
				
				else if (line.charAt(i) == '(' || line.charAt(i) == '{' || line.charAt(i) == '[')
				{
					push(line.charAt(i));
				}
				
				else
				{
					
				}
			}
			
			if (empty())
				return true;
			else
				return false;
	}
	
	public boolean checkSymbolsBalanced(String str)
	{
		Stack<Character> stk = new Stack<>();
		
		if (str == null || str.length() == 0)
			return true;
		
		// check till the end of the input
		for (int i=0; i<str.length(); i++){
			if (str.charAt(i) == ')'){
				if (!stk.isEmpty() && stk.peek() == '(')
					stk.pop();
				else
					return false;
			}
			
			else if (str.charAt(i) == ']'){
				if (!stk.isEmpty() && stk.peek() == '[')
					stk.pop();
				else
					return false;
			}
			
			else if (str.charAt(i) == '}'){
				if (!stk.isEmpty() && stk.peek() == '{')
					stk.pop();
				return false;
			}
			
			else if (str.charAt(i) == '(' || str.charAt(i) == '[' || str.charAt(i) == '{')
				stk.push (str.charAt(i));
			
			// else ignore
		}
		
		if (stk.isEmpty())
			return true;
		else 
			return false;
	}
}

public class MatchingParenthesis
{
	public static void main(String[] args)
	{
		Scanner in = new Scanner(System.in);
		StackImplementation s = new StackImplementation();		
		System.out.println("Enter the input string : ");
		String line = in.nextLine();
		if (s.checkSymbolsBalanced(line))
			System.out.println("The semantics of the program are proper ");
		else
			System.out.println("The semantics of the program are not proper ");
	}
}
