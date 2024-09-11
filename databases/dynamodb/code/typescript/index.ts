import {
  GetItemCommand,
  DynamoDBClient,
  DescribeTableCommand,
  DeleteTableCommand,
  CreateTableCommand,
  QueryCommand,
  PutItemCommand
} from '@aws-sdk/client-dynamodb';
import { inspect } from 'util';
import type {
  PutItemCommandInput,
  CreateTableCommandInput
} from '@aws-sdk/client-dynamodb';

const config = {
  endpoint: 'http://localhost:8000'
};

const TableName = 'Products';
const client = new DynamoDBClient(config);

async function dropTableIfExists() {
  try {
    const describeInput = { TableName: TableName };
    const describeCommand = new DescribeTableCommand(describeInput);
    const _describeResponse = await client.send(describeCommand);

    const deleteInput = { TableName: TableName };
    const deleteCommand = new DeleteTableCommand(deleteInput);
    const deleteResponse = await client.send(deleteCommand);
    console.log(
      `Deleted table ${TableName}. HTTP status code: ${deleteResponse.$metadata.httpStatusCode}`
    );
  } catch (error) {
    console.log('Table did not exist');
  }
}

async function createTable() {
  // https://docs.aws.amazon.com/AWSJavaScriptSDK/v3/latest/client/dynamodb/command/CreateTableCommand/
  const createTableInput: CreateTableCommandInput = {
    TableName: TableName,
    // define the attributes of the table. Attrs used in keys must also be defined here
    AttributeDefinitions: [
      { AttributeName: 'account_id', AttributeType: 'S' },
      { AttributeName: 'product_id', AttributeType: 'S' }
    ],

    // Define the primary key (partition key + optional sort key)
    // HASH = partition key
    // RANGE = sort key
    KeySchema: [
      { AttributeName: 'account_id', KeyType: 'HASH' }, // must have a partition key
      { AttributeName: 'product_id', KeyType: 'RANGE' } // optional sort key
    ],
    ProvisionedThroughput: {
      ReadCapacityUnits: 1,
      WriteCapacityUnits: 1
    },
    Tags: [
      {
        Key: 'Owner',
        Value: 'Eoin Test 12'
      }
    ]

    // Create a global secondary index (max 20 per table)
    // GlobalSecondaryIndexes: [
    // 	{
    // 		IndexName: 'title-index',
    // 		KeySchema: [
    // 			{ AttributeName: 'title', KeyType: 'HASH' }
    // 		],
    // 		Projection: {
    // 			ProjectionType: 'ALL' // which attrs should be copied/"projected" from the table into the index
    // 		},
    // 		ProvisionedThroughput: {
    // 			ReadCapacityUnits: 1,
    // 			WriteCapacityUnits: 1
    // 		}
    // 	}
    // ]

    // create a local secondary index (max 5 per table)
    // LocalSecondaryIndexes: [
    // 	{
    // 		IndexName: 'title-index',
    // 		KeySchema: [
    // 			{ AttributeName: 'account_id', KeyType: 'HASH' },
    // 			{ AttributeName: 'title', KeyType: 'RANGE' }
    // 		],
    // 		Projection: {
    // 			ProjectionType: 'ALL' // which attrs should be copied/"projected" from the table into the index
    // 		}
    // 	}
    // ]
  };
  const createTableCommand = new CreateTableCommand(createTableInput);
  const createTableResponse = await client.send(createTableCommand);
  console.log(
    `Created table ${TableName}. HTTP status code: ${createTableResponse.$metadata.httpStatusCode}`
  );
}

async function addItems() {
  const items: PutItemCommandInput[] = [
    {
      TableName: TableName,
      ReturnConsumedCapacity: 'TOTAL',
      Item: {
        account_id: { S: '93d0' },
        product_id: { S: '0001' },
        title: { S: 'Blue & Black Dress' },
        colours: { SS: ['blue', 'black'] },
        likes: { N: '0' }
      }
    },
    {
      TableName: TableName,
      ReturnConsumedCapacity: 'TOTAL',
      Item: {
        account_id: { S: '93d0' },
        product_id: { S: '0002' },
        title: { S: 'White & Gold Dress' },
        colours: { SS: ['white', 'gold'] },
        likes: { N: '0' }
      }
    }
  ];

  for (const item of items) {
    const command = new PutItemCommand(item);
    const response = await client.send(command);
    console.log(
      `Created item: ${item?.Item?.title?.S}. HTTP status code: ${response.$metadata.httpStatusCode}`
    );
  }
}

async function getSingleItem() {
  console.log('Getting single item');
  try {
    const input = {
      Key: {
        account_id: {
          S: '93d0'
        },
        product_id: {
          S: '0001'
        }
      },
      TableName: TableName
    };
    const command = new GetItemCommand(input);
    const response = await client.send(command);
    console.log(`Item: ${JSON.stringify(response.Item)}`);
  } catch (error) {
    console.error(error);
  }
}

async function doQuery1() {
  console.log('Doing query 1');
  try {
    // https://docs.aws.amazon.com/AWSJavaScriptSDK/v3/latest/client/dynamodb/command/QueryCommand/
    const input = {
      // The table name to query
      TableName: TableName,

      //The name of an index to query. This index can be any local secondary
      //index or global secondary index on the table. Note that if you use the
      //IndexName parameter, you must also provide TableName.
      // IndexName: 'title-index',

      /*
				The condition that specifies the key values for items to be retrieved by
				the Query action.

				The condition must perform an equality test on a single partition key
				value.

				The condition can optionally perform one of several comparison tests on
				a single sort key value. This allows Query to retrieve one item with a
				given partition key value and sort key value, or several items that have
				the same partition key value but different sort key values.

				https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Condition.html
				https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Query.KeyConditionExpressions.html
			*/
      // KeyConditionExpression: 'account_id = :accId', // search only on partition key (must exact match)

      // exact match partition key and exact match sort key
      // KeyConditionExpression: 'account_id = :accId AND product_id = :prodId',

      // exact match partition key and exact match sort key
      // KeyConditionExpression: 'account_id = :accId AND product_id = :prodId',
      // KeyConditionExpression: 'account_id EQ :accId AND product_id EQ :prodId', // breaks

      // exact match partition key and prefix match sort key
      KeyConditionExpression:
        'account_id = :accId and begins_with(product_id, :prodId)',

      /*
				One or more values that can be substituted in an expression.

				Use the : (colon) character in an expression to dereference an attribute
				value. For example, suppose that you wanted to check whether the value
				of the ProductStatus attribute was one of the following:

				Available | Backordered | Discontinued

				You would first need to specify ExpressionAttributeValues as follows:

				{ ":avail":{"S":"Available"}, ":back":{"S":"Backordered"}, ":disc":{"S":"Discontinued"} }

				You could then use these values in an expression, such as this:

				ProductStatus IN (:avail, :back, :disc)
			*/
      ExpressionAttributeValues: {
        ':accId': {
          S: '93d0'
        },
        ':prodId': {
          S: '0001'
        }
      }

      /*
				A string that identifies one or more attributes to retrieve from the
				table. These attributes can include scalars, sets, or elements of a JSON
				document. The attributes in the expression must be separated by commas.

				If no attribute names are specified, then all attributes will be
				returned. If any of the requested attributes are not found, they will
				not appear in the result.
			*/
      // ProjectionExpression: 'SongTitle',

      /*
				A string that contains conditions that DynamoDB applies after the Query
				operation, but before the data is returned to you. Items that do not
				satisfy the FilterExpression criteria are not returned.
			*/
      // FilterExpression: 'blah',
    };
    const command = new QueryCommand(input);
    const response = await client.send(command);
    console.log(
      inspect(response, { showHidden: false, depth: null, colors: true })
    );
  } catch (error) {
    console.error(error);
  }
}
await dropTableIfExists();
await createTable();
await addItems();
await getSingleItem();
await doQuery1();
