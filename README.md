# FOA - Feedback On Anything

Collection of Postman requests:

```json
{
	"variables": [],
	"info": {
		"name": "foa",
		"_postman_id": "8d15c6fe-8fd5-4f95-1040-51255030baa2",
		"description": "",
		"schema": "https://schema.getpostman.com/json/collection/v2.0.0/collection.json"
	},
	"item": [
		{
			"name": "home",
			"request": {
				"url": "http://localhost:3000/",
				"method": "GET",
				"header": [],
				"body": {},
				"description": ""
			},
			"response": []
		},
		{
			"name": "all feedback",
			"request": {
				"url": "http://localhost:3000/feedback",
				"method": "GET",
				"header": [],
				"body": {},
				"description": ""
			},
			"response": []
		},
		{
			"name": "feedback",
			"request": {
				"url": "http://localhost:3000/feedback/70c57beb-ce8e-4ea1-83e0-4e26da086280",
				"method": "GET",
				"header": [],
				"body": {},
				"description": ""
			},
			"response": []
		},
		{
			"name": "new feedback",
			"request": {
				"url": "http://localhost:3000/feedback",
				"method": "POST",
				"header": [
					{
						"key": "Content-Type",
						"value": "application/json",
						"description": ""
					}
				],
				"body": {
					"mode": "raw",
					"raw": "{\n\t\"target\": \"5749CF55-CB73-4B0C-A222-1959BBD6038A\",\n\t\"targetOwner\": \"3DE0188A-CEDE-4305-8C73-2D643F0A6ED9\",\n\t\"feedbackGiver\": \"CBC041D9-F503-4053-AEA4-EC23C057CE50\",\n\t\"experience\": \"c\",\n\t\"comment\": \"d\"\n}"
				},
				"description": ""
			},
			"response": []
		},
		{
			"name": "update feedback",
			"request": {
				"url": "http://localhost:3000/feedback/70c57beb-ce8e-4ea1-83e0-4e26da086280",
				"method": "PUT",
				"header": [
					{
						"key": "Content-Type",
						"value": "application/json",
						"description": ""
					}
				],
				"body": {
					"mode": "raw",
					"raw": "{\n\t\"target\": \"5749CF55-CB73-4B0C-A222-1959BBD6038A\",\n\t\"targetOwner\": \"3DE0188A-CEDE-4305-8C73-2D643F0A6ED9\",\n\t\"feedbackGiver\": \"CBC041D9-F503-4053-AEA4-EC23C057CE50\",\n\t\"experience\": \"e\",\n\t\"comment\": \"f\"\n}"
				},
				"description": ""
			},
			"response": []
		},
		{
			"name": "delete feedback",
			"request": {
				"url": "http://localhost:3000/feedback/70c57beb-ce8e-4ea1-83e0-4e26da086280",
				"method": "DELETE",
				"header": [
					{
						"key": "Content-Type",
						"value": "application/json",
						"description": ""
					}
				],
				"body": {
					"mode": "raw",
					"raw": ""
				},
				"description": ""
			},
			"response": []
		}
	]
}
```