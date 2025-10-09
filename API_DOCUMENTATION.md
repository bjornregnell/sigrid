# Sigrid API Documentation

This document describes the REST API endpoints added to Sigrid for frontend integration.

## Base URL

When running locally: `http://localhost:8091/api/`

## Endpoints

### Room Management

#### GET /api/rooms
List all active rooms.

**Response:**
```json
{
  "rooms": [
    {
      "course": "PGK",
      "name": "Hacke",
      "supervisors": [{"name": "alice", "number": 1, "id": "alice-1"}],
      "students": [{"name": "bob", "number": 1, "id": "bob-1"}],
      "helpQueue": [
        {
          "user": {"name": "bob", "number": 1, "id": "bob-1"},
          "timestamp": {"year": 2025, "month": 10, "day": 9, "hour": 14, "minute": 30, "second": 0, "display": "2025-10-09 14:30:00"},
          "minutesWaited": 5
        }
      ],
      "approvalQueue": [],
      "created": {"year": 2025, "month": 10, "day": 9, "hour": 14, "minute": 0, "second": 0, "display": "2025-10-09 14:00:00"},
      "isActive": true,
      "isExpired": false,
      "maxQueuingTime": 5
    }
  ]
}
```

#### GET /api/rooms/{course}/{room}
Get details for a specific room.

**Parameters:**
- `course` (path): Course code (e.g., "PGK", "DOD")
- `room` (path): Room name (e.g., "Hacke", "Pluto")

**Response:** Room object (same structure as in rooms array above)

**Error Response (404):**
```json
{
  "error": "RoomNotFound",
  "message": "Room Hacke in course PGK does not exist"
}
```

---

### User Management

#### GET /api/users/{userId}
Get user status and current room.

**Parameters:**
- `userId` (path): User ID (format: "name-number", e.g., "alice-1")

**Response:**
```json
{
  "user": {"name": "alice", "number": 1, "id": "alice-1"},
  "room": {
    "course": "PGK",
    "name": "Hacke",
    ...
  }
}
```

**Note:** `room` will be `null` if user is not in any room.

---

### Student Operations

#### POST /api/student/login
Log in as a student and join a room.

**Query Parameters:**
- `name` (required): Student's first name
- `course` (required): Course code
- `room` (required): Room name

**Response:**
```json
{
  "user": {"name": "bob", "number": 1, "id": "bob-1"},
  "room": {
    "course": "PGK",
    "name": "Hacke",
    ...
  }
}
```

**Notes:**
- If a user with the same name exists, a number suffix is auto-incremented (bob-1, bob-2, etc.)
- Creates the room if it doesn't exist
- Automatically purges expired rooms and orphaned users

#### POST /api/student/state
Update student state (request help, ready for approval, working, or exit).

**Query Parameters:**
- `userid` (required): User ID (from login response)
- `course` (required): Course code
- `room` (required): Room name
- `state` (required): One of: `work`, `help`, `ready`, `exit`

**Valid States:**
- `work` - Student is working (removes from queues)
- `help` - Student needs help (adds to help queue)
- `ready` - Student is ready for approval (adds to approval queue)
- `exit` - Student leaves (removes user from system)

**Response:**
```json
{
  "room": { ... },
  "message": "Status updated"
}
```

**Error Response (400):**
```json
{
  "error": "InvalidState",
  "message": "State 'invalid' is not valid. Valid states: work, help, ready, exit"
}
```

---

### Supervisor Operations

#### POST /api/supervisor/login
Log in as a supervisor and join a room.

**Query Parameters:**
- `name` (required): Supervisor's first name
- `course` (required): Course code
- `room` (required): Room name

**Response:** Same as student login

#### POST /api/supervisor/action
Perform supervisor actions (pop queues, remove users, merge rooms, etc.).

**Query Parameters:**
- `userid` (required): Supervisor user ID
- `course` (required): Course code
- `room` (required): Room name
- `action` (required): Action to perform (see below)
- `targetuser` (optional): Target user ID for `removeuser` action
- `otherroom` (optional): Other room name for `mergeroom` action

**Valid Actions:**
- `supervising` - Get current room state
- `pophelp` - Remove first student from help queue
- `popready` - Remove first student from approval queue
- `clearhelp` - Clear entire help queue
- `clearready` - Clear entire approval queue
- `removeuser` - Remove a specific user (requires `targetuser` parameter)
- `mergeroom` - Merge another room into this one (requires `otherroom` parameter)
- `gone` - Supervisor leaves the room
- `purge` - Delete the entire room

**Response:**
```json
{
  "room": { ... },
  "message": "Help queue popped"
}
```

**Examples:**

Pop help queue:
```
POST /api/supervisor/action?userid=alice-1&course=PGK&room=Hacke&action=pophelp
```

Remove a user:
```
POST /api/supervisor/action?userid=alice-1&course=PGK&room=Hacke&action=removeuser&targetuser=bob-1
```

Merge rooms:
```
POST /api/supervisor/action?userid=alice-1&course=PGK&room=Hacke&action=mergeroom&otherroom=Pluto
```

**Error Responses:**
```json
{
  "error": "QueueEmpty",
  "message": "Help queue is empty or room not found"
}
```

```json
{
  "error": "InvalidOperation",
  "message": "Cannot remove yourself"
}
```

---

## Data Models

### User
```json
{
  "name": "alice",
  "number": 1,
  "id": "alice-1"
}
```

### Date
```json
{
  "year": 2025,
  "month": 10,
  "day": 9,
  "hour": 14,
  "minute": 30,
  "second": 0,
  "display": "2025-10-09 14:30:00"
}
```

### Room
```json
{
  "course": "PGK",
  "name": "Hacke",
  "supervisors": [User, ...],
  "students": [User, ...],
  "helpQueue": [QueueItem, ...],
  "approvalQueue": [QueueItem, ...],
  "created": Date,
  "isActive": true,
  "isExpired": false,
  "maxQueuingTime": 5
}
```

### QueueItem
```json
{
  "user": User,
  "timestamp": Date,
  "minutesWaited": 5
}
```

---

## Error Handling

All errors return HTTP status 400 (Bad Request) with a JSON body:

```json
{
  "error": "ErrorCode",
  "message": "Human-readable error message"
}
```

**Common Error Codes:**
- `RoomNotFound` - The specified room doesn't exist
- `UserNotFound` - The specified user doesn't exist
- `InvalidState` - Invalid state parameter
- `InvalidAction` - Invalid supervisor action
- `LoginFailed` - Could not complete login
- `UpdateFailed` - Could not update state
- `QueueEmpty` - Attempted to pop from empty queue
- `InvalidOperation` - Operation not allowed (e.g., removing yourself)
- `InvalidUserId` - Malformed user ID
- `MergeFailed` - Could not merge rooms

---

## Implementation Notes

### Backward Compatibility
All existing HTML endpoints remain functional:
- `/sigrid/*` - Student interface
- `/beppe/*` - Supervisor interface
- `/sigrid/monitor` - Monitor dashboard

The HTML and API endpoints share the same underlying database, so they can be used interchangeably.

### Input Validation
All input parameters are validated and sanitized:
- Names: Letters only, max 25 characters
- Course codes: Alphanumeric, max 25 characters (with mapping: EDAA45→PGK, EDAA60/EITA65→DOD)
- Room names: Alphanumeric, max 20 characters, capitalized
- User IDs: Format "name-number" (e.g., "alice-1")

### Room Lifecycle
- Rooms are automatically created when first user joins
- Rooms are marked removable when empty or after 10 hours
- Expired/empty rooms are purged on login requests

### Thread Safety
All operations use atomic updates via `AtomicKeyValueStore`, ensuring thread-safe concurrent access.

---

## Example Frontend Flow

### Student Workflow
1. **Login:** `POST /api/student/login?name=bob&course=PGK&room=Hacke`
   - Save `user.id` from response
2. **Request help:** `POST /api/student/state?userid=bob-1&course=PGK&room=Hacke&state=help`
3. **Back to work:** `POST /api/student/state?userid=bob-1&course=PGK&room=Hacke&state=work`
4. **Exit:** `POST /api/student/state?userid=bob-1&course=PGK&room=Hacke&state=exit`

### Supervisor Workflow
1. **Login:** `POST /api/supervisor/login?name=alice&course=PGK&room=Hacke`
   - Save `user.id` from response
2. **Monitor room:** `GET /api/rooms/PGK/Hacke` (poll periodically)
3. **Help student:** `POST /api/supervisor/action?userid=alice-1&course=PGK&room=Hacke&action=pophelp`
4. **Exit:** `POST /api/supervisor/action?userid=alice-1&course=PGK&room=Hacke&action=gone`

### Monitor Dashboard
1. **List all rooms:** `GET /api/rooms` (poll every 10 seconds)
